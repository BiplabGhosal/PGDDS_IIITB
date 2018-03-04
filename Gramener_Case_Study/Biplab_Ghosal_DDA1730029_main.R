#Below are the libraries used to clean, modify, analyse and visualize the data.
#Library lubridate,corrplot,VIM and treemap are installed and then used for data processing
# and visualization.

#Install the packages if not available in the system.

#install.packages("lubridate")
#install.packages("corrplot")
#install.packages("VIM")
#install.packages("treemap")

library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot, quietly = TRUE)
library(VIM)
library(treemap)


#Read loan data set into a dataframe called SrcData.
SrcData = read.csv('loan.csv', header = T, stringsAsFactors = F,check.names = F)



#####################################Reusable fuctions#################################################

# Create reusable function to generate summaries grouping by ... and showing how much loan amount and how many loans were issued by groups

sumAmnt = function(x, ...) {
  x %>% group_by(., ...) %>%
    summarise(., total_issued = round(sum(loan_amnt/1),1),
              number_of_loans = round(n()),mean   = round(round(sum(loan_amnt/1),1) / n() * 100, 2))
}


# Create reusable function to generate summaries grouping by ... and showing usefull statics about each group

sumStat = function(x, ...) {
  x %>% group_by(., ...) %>%
    summarise(., median = round(median(loan_amnt/1)),
              average = round(mean(loan_amnt/1)),
              stdev = round(sd(loan_amnt/1)))
}

# calculating retrun on investment
roi = function(x, ...){
  x %>% group_by(...) %>%
    summarize(., roi= round(sum((total_pymnt / funded_amnt) * 100) / n(), 2))
}


# used to generate summaries grouping by ... and showing how much loan amount is in defaulter status
#This is useful for segmented analysis

sumPerSatus = function(x, ...){
  x %>% group_by(...) %>%
    summarize(., charged_off   = round(sum(loan_status == "Charged Off") / n() * 100, 2),
              full_paid   = round(sum(loan_status == "Fully Paid") / n() * 100, 2),
              Current   = round(sum(loan_status == "Current") / n() * 100, 2),
              avg_grade = sub_grade_vec[mean(SrcData_score)])
}


#############################################################################################



###############################################################################################
#Data cleansing, Modifying, and adding new columns to the original data to undestand the data:
###############################################################################################


#remove all columns which have only NA values.
SrcData <- SrcData[,colSums(is.na(SrcData))<nrow(SrcData)]


#unique(df$term)
#Check for duplicate values present in the dataframe.
nrow(unique(SrcData))  #No duplicate row present.

#Transform the dataframe to remove extra space from term variable.

SrcData <- transform(SrcData,term = str_trim(term))  

unique(SrcData$term) #only two categories present. "36 months" "60 months"

#Let's convert it two a factor.

SrcData$term <- as.factor(SrcData$term)

#Check for policy_code != 1 i.e. to analyse only public data.

unique(SrcData$policy_code) #only public data is present, so no need to remove any data for now.

#Check for loan status ...

unique(SrcData$loan_status) 

#3 types of loan status present so making this as factor categorical variable.

SrcData$loan_status <- as.factor(SrcData$loan_status)



#Check NA values in issue_y #0
sum(is.na(SrcData$issue_d)) 

#Checking the NA value in issue date 
filter(SrcData,is.na(issue_d))

unique(SrcData$issue_d)

# No na value or data annomaly present in issue_d

# adding issue year, quarter, month to the original dataframe...

Months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
SrcData = mutate(SrcData, issue_y = as.integer(substr(issue_d, 5, 6)),
                 issue_m = match(substr(issue_d, 1, 3),Months),
                 issue_ym = (issue_y) * 100 + issue_m,
                 issue_q = ceiling(issue_m / 3),
                 issue_yq = paste((issue_y), "-Q", issue_q, sep = ""),
                 n = 1)


SrcData<-mutate(SrcData,issue_d_new = dmy(paste0("01-",SrcData$issue_d)),earliest_cr_line_new= dmy(paste0("01-",SrcData$earliest_cr_line)))


# Add a feature "matured date" for Loans that have or would have matured date
SrcData = mutate(SrcData, matured_dt = (issue_ym + ifelse(term == " 36 months", 300, 500)))


#Check the purpose of loan 
unique(SrcData$purpose)

#[1] "credit_card"        "car"                "small_business"     "other"             
#[5] "wedding"            "debt_consolidation" "home_improvement"   "major_purchase"    
#[9] "medical"            "moving"             "vacation"           "house"             
#[13] "renewable_energy"   "educational"   

#We can group some of these together to reduce number of purpose.

SrcData = mutate(SrcData, purpose_grp = ifelse(purpose == "credit_card" | 
                                                 purpose == "debt_consolidation", "debt",
                                               ifelse(purpose == "car" | 
                                                        purpose == "major_purchase" | 
                                                        purpose == "vacation" | 
                                                        purpose == "wedding" | 
                                                        purpose == "medical" | 
                                                        purpose == "other", "purchase",
                                                      ifelse(purpose == "house" | 
                                                               purpose == "home_improvement" | 
                                                               purpose == "moving" | 
                                                               purpose == "renewable_energy", "house", purpose))))


#Let's do simmilar excercise for Home ownership.

unique(SrcData$home_ownership)
#"RENT"     "OWN"      "MORTGAGE" "OTHER"    "NONE"

# combine NONE and OTHER together to reduce category which are similar
SrcData = mutate(SrcData, home = ifelse(home_ownership == "NONE", "OTHER", home_ownership))


# give loan data grade numeric values , later if we can do some power law distribution check.

sub_grade_vec = unique(SrcData$sub_grade) %>% .[order(., decreasing = T)]
SrcData = mutate(SrcData, SrcData_score = match(sub_grade, sub_grade_vec))


# creating delinq_2yrs buckets:
SrcData = mutate(SrcData, delinq_bucket = ifelse(delinq_2yrs >= 2, "2+", delinq_2yrs))

# inq_last_6mths buckets:
SrcData = mutate(SrcData, inq_bucket = ifelse(inq_last_6mths >= 7, "7+", 
                                              ifelse(inq_last_6mths >= 5, "5-6", 
                                                     ifelse(inq_last_6mths >= 3, "3-4", 
                                                            ifelse(inq_last_6mths >= 1, "1-2", 0)))))

#Create new variable for interest rate rounding up original interest rate.
SrcData <- mutate(SrcData,int_rate_approx =  ceiling(as.numeric(gsub("^(.*?)%$", "\\1", int_rate))))

#Make a factor of annual income devided by 10000.
SrcData <- mutate(SrcData,annual_inc_factor = round((annual_inc/10000),0))


#"n/a" value present in the employee length, 
unique(SrcData$emp_length)

#get the average loan amount by emp length. 
sumAmnt(SrcData,emp_length)

#As n/a group mean is very cloase to <1 year group, lets impute n/a value to < 1 year.

SrcData[SrcData$emp_length == "n/a",'emp_length'] <- "< 1 year"

#Create 3 different dataframe based on loan status.
charged_off <- filter(SrcData,loan_status=="Charged Off")

fully_paid <- filter(SrcData,loan_status=="Fully Paid")


current <- filter(SrcData,loan_status=="Current")



##################################################################################
############### End Of primary Data Cleansing , and adding new variable ##################
##################################################################################



##################################################################################
##################### Start Data analysing in more detail ########################
##################################################################################


#See the summary of the dataframe.
summary(SrcData)


#Summary of loan amount
summary(SrcData$loan_amnt)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   500    5500   10000   11219   15000   35000 

#Creaing a boxPlot to view the data
boxplot(SrcData$loan_amnt)

# Total amount of loans
sum(SrcData$loan_amnt) #445602650

# From the below box Plot we can clearly see some outlier , next step is to impute using median

boxplot(SrcData$installment)

#median is 280.22
median(SrcData$installment)

#find Q3 value using quantile fuction to impute greater Q3 + 1.5*IQR 
quantile(SrcData$installment) 
#0%     25%     50%     75%    100% 
#15.69  167.02  280.22  430.78 1305.19 

sum(SrcData$installment > (430.78 + 1.5*IQR(SrcData$installment)))
#1245 outliers

#imputing outliers using median value
SrcData$installment[which(SrcData$installment > (430.78 + 1.5*IQR(SrcData$installment)))] <- 280.22
#


# From the below box Plot we can clearly see some outlier , next step is to impute using median

boxplot(SrcData$annual_inc)

#median is 59000
median(SrcData$annual_inc)

#find Q3 value using quantile fuction to impute greater Q3 + 1.5*IQR 
quantile(SrcData$annual_inc) 
#0%     25%     50%     75%    100% 
#4000   40404   59000   82300 6000000 

sum(SrcData$annual_inc > (82300 + 1.5*IQR(SrcData$annual_inc)))
#1245 outliers

#imputing outliers using median value
SrcData$annual_inc[which(SrcData$annual_inc > (82300 + 1.5*IQR(SrcData$annual_inc)))] <- 59000
#

# Looking at total loan amnts per loan_status
# Report showing the amounts and number of loans in each category, here the latest status of the loans

sumAmnt(SrcData, loan_status)
# A tibble: 3 x 3
#loan_status total_issued number_of_loans
#       <fctr>        <dbl>           <dbl>
#  1 Charged Off     68111375            5627
#  2     Current     19441550            1140
#  3  Fully Paid    358049725           32950


# Report showing some statistics for each category, the latest status of the loans
sumStat(SrcData, loan_status) 

# A tibble: 3 x 4
#loan_status median average stdev
#       <fctr>  <dbl>   <dbl> <dbl>
#1 Charged Off  10000   12104  8086
#2     Current  15825   17054  8651
#3  Fully Paid   9600   10866  7200



# Summary by issue year
# Report showing the amounts and number of loans in each category, here the year of issuance
sumAmnt(SrcData, issue_y)

# A tibble: 5 x 3
#issue_y total_issued number_of_loans
#<int>        <dbl>           <dbl>
#1       7      2219275             251
#2       8     14390275            1562
#3       9     46436325            4716
#4      10    122050200           11532
#5      11    260506575           21656


#following report showing yea-wise statics of loan
sumStat(SrcData, issue_y)

# A tibble: 5 x 4
#issue_y median average stdev
#<int>  <dbl>   <dbl> <dbl>
#1       7   6500    8842  6448
#2       8   7500    9213  5774
#3       9   9000    9847  5934
#4      10   9600   10584  6602
#5      11  10000   12029  8159

#In the below report we will see year and loan status wise number of loan percentage.
sumPerSatus(SrcData, issue_y)

# A tibble: 5 x 5
#issue_y charged_off full_paid Current avg_grade
#<int>       <dbl>     <dbl>   <dbl>     <chr>
#1       7       17.93     82.07    0.00        C2
#2       8       15.81     84.19    0.00        C1
#3       9       12.60     87.40    0.00        C1
#4      10       12.88     87.12    0.00        C2
#5      11       15.04     79.70    5.26        C1

# From above report we can see issue year 9 and 10 has steady rate of 12% deefaulter, which 
# is not so conclusive.

agg_issue_yq <- sumPerSatus(SrcData, issue_yq)

Plot1 <- agg_issue_yq %>% ggplot(aes(x=issue_yq,y=charged_off)) + 
  geom_bar(stat="identity",fill = 'steelblue') +
  geom_text(stat='identity',aes(label=paste(as.character(charged_off),'%')),vjust=-1) + 
  labs(x="issue year quarter", y="percentage of Loans number", title = "Defaulter: Issued Loans of Different issue year quarter") +
  theme_bw() 

#Show the Plot
Plot1

Plot2 <- agg_issue_yq %>% ggplot(aes(x=issue_yq,y=full_paid)) + 
  geom_bar(stat="identity",fill = 'aquamarine3') +
  geom_text(stat='identity',aes(label=paste(as.character(full_paid),'%')),vjust=0.5,hjust=1,angle = 90,size=3) + 
  labs(x="issue year quarter", y="percentage of Loans number", title = "Paid: Issued Loans of Different issue year quarter") +
  theme_bw() 

#Show the Plot
Plot2
#the above Plot showing year- quarter wise number of loan percentage. No relation with charged off for quarter


# Looking at grade statistics:
# Report showing the amounts and number of loans in each category, here grades
sumAmnt(SrcData, grade)


# A tibble: 7 x 3
#grade total_issued number_of_loans
#<chr>        <dbl>           <dbl>
#1     A     86982400           10085
#2     B    133651350           12020
#3     C     89115825            8098
#4     D     65160400            5307
#5     E     45037900            2842
#6     F     19263100            1049
#7     G      6391675             316



# Report showing some statistics for each category, here grades
sumStat(SrcData, grade)
# A tibble: 7 x 4
#grade median average stdev
#<chr>  <dbl>   <dbl> <dbl>
#1     A   7500    8625  5332
#2     B  10000   11119  7221
#3     C   9725   11005  7296
#4     D  10000   12278  7629
#5     E  15000   15847  8989
#6     F  18000   18363  9180
#7     G  20250   20227  8517

#Now generate new report to see status wise percentage of loan requested,
sumPerSatus(SrcData, grade)

# A tibble: 7 x 5
#grade charged_off full_paid Current avg_grade
#<chr>       <dbl>     <dbl>   <dbl>     <chr>
#1     A        5.97     93.63    0.40        A4
#2     B       11.86     85.27    2.87        B4
#3     C       16.63     80.11    3.26        C3
#4     D       21.07     74.75    4.18        D3
#5     E       25.16     68.54    6.30        E3
#6     F       30.41     62.63    6.96        F3
#7     G       31.96     62.66    5.38        G3

Plot3 <- sumPerSatus(SrcData, grade) %>% ggplot(aes(x=grade,y=charged_off)) + 
  geom_bar(stat="identity",fill='chartreuse3') +
  geom_text(stat='identity',aes(label=paste(as.character(charged_off),'%')),vjust=-1) + 
  labs(title ='Loan requested by grade : defaulter',x = 'Grade', y = 'Total requested(%)') + theme_bw()

#show the Plot now for grade-wise defaulter loan
Plot3
#Here in Plot3 we see direct relation with grade. Number of defaulter getting increased from A to G

# Report showing the amounts and number of loans in each category, here the home ownership status of the borrower.
sumAmnt(SrcData, home)

# A tibble: 4 x 3
#home total_issued number_of_loans
#<chr>        <dbl>           <dbl>
#1 MORTGAGE    224094000           17659
#2    OTHER      1061775             101
#3      OWN     31365150            3058
#4     RENT    189081725           18899



# Report showing some statistics for each category, here the home ownership status of the borrower.
sumPerSatus(SrcData, home)

Plot4 <- sumPerSatus(SrcData, home) %>% ggplot(aes(x=home,y=charged_off)) + 
  geom_bar(stat="identity",fill='steelblue',width = 0.5) +
  geom_text(stat='identity',aes(label=paste(as.character(charged_off),'%')),vjust=-1) + 
  labs(title ='Loan requested by home : defaulter',x = 'Home', y = 'Total requested(%)') + theme_bw()

#show the Plot now for home type-wise defaulter loan count
Plot4

#Other home type has most defaulter present than Mortgage or OWN

# A tibble: 4 x 5
#home charged_off full_paid Current avg_grade
#<chr>       <dbl>     <dbl>   <dbl>     <chr>
#1 MORTGAGE       13.18     83.21    3.61        C1
#2    OTHER       17.82     82.18    0.00        C1
#3      OWN       14.49     82.80    2.71        C1
#4     RENT       15.02     82.76    2.22        C2

sumAmnt(SrcData, purpose_grp)
# A tibble: 5 x 3
#purpose_grp total_issued number_of_loans
#<chr>        <dbl>           <dbl>
#1           debt    296789450           23771
#2    educational      2213400             325
#3          house     44089700            4043
#4       purchase     77709125            9750
#5 small_business     24800975            1828

sumStat(SrcData, purpose_grp)

# A tibble: 5 x 4
#purpose_grp median average stdev
#<chr>  <dbl>   <dbl> <dbl>
#1           debt  11000   12485  7387
#2    educational   5400    6810  5141
#3          house   9000   10905  8009
#4       purchase   6000    7970  5990
#5 small_business  12000   13567  8622

sumPerSatus(SrcData, purpose_grp)

# A tibble: 5 x 5
#purpose_grp charged_off full_paid Current avg_grade
#<chr>       <dbl>     <dbl>   <dbl>     <chr>
#1           debt       13.92     83.18    2.90        C2
#2    educational       17.23     82.77    0.00        C1
#3          house       12.79     84.17    3.04        C1
#4       purchase       13.03     84.37    2.61        B5
#5 small_business       25.98     69.97    4.05        C4


Plot5 <- sumPerSatus(SrcData, purpose_grp) %>% ggplot(aes(x=purpose_grp,y=charged_off)) + 
  geom_bar(stat="identity",fill='darkcyan') +
  geom_text(stat='identity',aes(label=paste(as.character(charged_off),'%')),vjust=-1) + 
  labs(title ='Loan requested by purpose_grp : defaulter',x = 'purpose_grp', y = 'Total requested(%)') + theme_bw()

#show the Plot now for purpose_grp type-wise defaulter loan count
Plot5

#People took loans for small_business and education are mostly turnout to be a defaulter as per
# the above Plot.

Plot6 <- sumPerSatus(SrcData, sub_grade) %>% ggplot(aes(x=sub_grade,y=charged_off)) + 
  geom_bar(stat="identity",fill='darkorchid2') +
  geom_text(stat='identity',aes(label=paste(as.character(charged_off),'%')),vjust=0.5,
            hjust=1,angle=90,size=3) + 
  labs(title ='Loan requested by sub grade : defaulter',x = 'Sub Grade', y = 'Total requested(%)') + theme_bw()

#show the Plot now for sub grade type-wise defaulter loan count
Plot6

#F5 sub grade has most number of defaulter than any other grades.



#Lets Find out delinq_bucket corresponding loan status wise summary
sumPerSatus(SrcData, delinq_bucket)

# A tibble: 3 x 5
#delinq_bucket charged_off full_paid Current avg_grade
#<chr>       <dbl>     <dbl>   <dbl>     <chr>
#1             0       13.94     83.18    2.88        C1
#2             1       15.35     81.74    2.91        C4
#3            2+       18.24     79.29    2.48        D1

#From the above report we can't see any major reation so skipping Plot for the same


#Summarizing inquiry wise loan status percentage and from the report its clear that. customer who inquired
#more than 7 times or so highly defaulter.

sumPerSatus(SrcData, inq_bucket)

#Creating the following Plot to visualize the same/
Plot7 <- sumPerSatus(SrcData, inq_bucket) %>% ggplot(aes(x=inq_bucket,y=charged_off)) + 
  geom_bar(stat="identity",fill='darkcyan',width=0.5) +
  geom_text(stat='identity',aes(label=paste(as.character(charged_off),'%')),vjust=0.5,
            hjust=1,angle=90,size=3) + 
  labs(title ='Loan requested by inquiry number : defaulter',x = 'inq_bucket', y = 'Total requested(%)') + theme_bw()

#show the Plot now for inquery bucket-wise defaulter loan count
Plot7

# Number of enquiry is directly proportional with defaulter. less number of enquiry means less chance of being defaulter.

#Lets analyse emp lenth and if any relation with being defaulter.
sumPerSatus(SrcData, emp_length)

#seems there is not much relation between defaulter and emp_length


#Checking the histogram of dti for each of the loan status,


hist(charged_off$dti,breaks =100)
hist(fully_paid$dti,breaks =100)
hist(current$dti,breaks =100)

#Now ploting a density plot to find out the distribution of dti for loan requests per status.
Plot <- ggplot(SrcData,aes(x=dti, fill=loan_status)) + geom_density(alpha=0.33) + theme_bw()

Plot
#From the above plot we can see lower the dti lower the rist of being defaulter.

Total_Amount_defaulter <- charged_off %>% select(issue_d_new,funded_amnt) %>% na.omit() %>% group_by(issue_d_new) %>% 
  summarise(amount = sum(funded_amnt),na.rm = T, volume = n(), AvgAmount = amount/volume)


#Lets now see loan amount approved per month by year
Plot8 <- ggplot(Total_Amount_defaulter,aes(x=issue_d_new)) +
  geom_line(aes(y=amount), color = 'slateblue2',size=2)  +
  geom_point(aes(y=amount), color = 'darkviolet',size=3) +
  geom_text(stat='identity',aes(y=amount,label=volume),hjust = -2,angle=90,size=3)  +  
  labs(title = 'Default Loan Amount by Date',x = 'Date Issued', y = 'Amount') + theme_bw()


#Showing the Plot, and we can see over the years amount loan increased which is a good sign of new business.
Plot8


Plot9 <- ggplot(Total_Amount_defaulter,aes(x=issue_d_new, y=AvgAmount)) +
  geom_point(color = "blue") + 
  geom_smooth(color = "green", linetype = "dashed") +
  labs(title ='Average Loan Amount by Date',x = 'Date Issued', y = 'Avg Amount') + theme_bw()

Plot9
#The above graph shows the average amount of loan increased at a constant rate from 2007 to 2009 Q2,
#Then loan grow in decreasing rate. From 2010 to 2011, the average loan amount remained roughly unchanged.



Plot10 <- ggplot(charged_off,aes(x=issue_y)) + 
  geom_bar(aes(fill = grade)) + 
  facet_grid(~term) + 
  labs(title ='Loan reqested by year',x = 'Issued Year', y = 'Number of loan request') + theme_bw()

#From the above 2 bar charts we could observe, the number of 36-month loans is 
#greater than 60-month loans. Also for 36-month loans, the majority are in 
#grade A,B,C and D but for 60-month loans only a small percentage of loans are in grade A 
#and most of the loans are in grade B,C,D and E.

Plot11 <- ggplot(charged_off, aes(grade,int_rate_approx)) + 
  geom_boxplot(outlier.size = 0.5, color = "blue") +
  facet_grid(term ~ issue_y) + 
  labs(title = "Interest Rate Distribution by Grade", x = "Grade", y= "Interest Rate(%)")+ 
  theme_bw()

#Plot11 used to show interest rate distribution over the year by grade.

Plot12 <- ggplot(charged_off, aes(grade)) + 
  geom_bar(aes(fill = grade)) + 
  facet_wrap(~home) + 
  labs(x="Grade", y="Number of Loans", title = "Issued Loans of Different Home Ownership") +
  theme_bw()

#Plot12 used to see grade, home and number of loan requested for 
Plot12

#From this Plot12 we can conclude that loan requestor who is having OWN house/flat very 
#unlikely to be a defaulter.

prp_df <- SrcData %>% select(purpose_grp, loan_amnt) %>% na.omit() %>% group_by(purpose_grp) %>% 
  summarise(volume = n(), average_amnt = sum(as.numeric(loan_amnt), 
                                             rm.na = TRUE)/n())

#Make a heat map for purpose group wise loan volume
treemap(prp_df, index = "purpose_grp", vSize = "volume", vColor = "average_amnt", 
        type = "manual", palette = c("yellow", "green", "orange", "orange2", "firebrick"), algorithm = "squarified", sortID = "-size", 
        title = "Purposes of Loans", title.legend = "Avg_Amnt", fontfamily.labels = "serif", 
        fontsize.labels = 16, fontsize.legend = 10, fontface.labels = 1, position.legend = "bottom", 
        force.print.labels = T, border.col = "white"
)

#debt consolidation is the most common reason for borrowing. 
#Most consumers choose to consolidate debt to enjoy lower borrowing costs. 
#Notice that there are three variables in the above tree map: purposes, average amount of 
#a loan, and the total volume of loans. It helps give an overall view of the relationship 
#between purposes of loans and the volume and amount of loans. The various sizes in the 
#above tree map are directly proportional to the volume of loans with different purposes. 



### missing value
aggr(SrcData, prop = T, number = F, label = T, gap = T, only.miss = T)

#Correlation check
#cor_var_name <- c("loan_amnt", "int_rate_approx", "installment", "sub_grade", "annual_inc", 
#"issue_d_new", "dti", "earliest_cr_line_new", "open_acc", "total_acc", "total_pymnt", 
#"total_rec_prncp", "total_rec_int")

cor_var_name <-c("loan_amnt", "int_rate_approx", "installment", "sub_grade", "annual_inc", 
                 "issue_d_new", "dti", "earliest_cr_line_new", "open_acc", "total_acc", "total_pymnt", 
                 "total_rec_prncp", "total_rec_int","annual_inc","dti","delinq_2yrs","inq_last_6mths","SrcData_score")


#Bellow process is used to show correlation between multiple variables.

cor_var <- select(charged_off, one_of(cor_var_name))
cor_var <- cor_var[complete.cases(cor_var),]

cor_var$credit_tm <- as.numeric(cor_var$issue_d - cor_var$earliest_cr_line_new)

cor_var$num_subgrade <- as.numeric(as.factor(cor_var$sub_grade))

cor_var <- select(cor_var, -sub_grade, -issue_d_new, -earliest_cr_line_new)

summary(cor_var)

M <- cor(cor_var)  # transfer to matrix 



corrplot(M, method = "number", title = "Correlation Map of Subgrade & Factors", 
         type = "lower", order = "FPC", number.cex = 0.5, tl.cex = 0.8, mar=c(0,0,2,0))

#From the above Plot we can see strong relation between Srcdata_score i.e. subgrade rank and interest rate.
#Below is the graph representation of the same .

Plot13 <- ggplot(charged_off, aes(SrcData_score,int_rate_approx)) + geom_point()  + 
  geom_smooth()

Plot13

#From the above plt we can see sirect relation between interst rate and sub grade.
#People on lower grade, subgrade i.e. from A-F tends to look for lower interest rate.
# But those people are very like to become defaulter as we can see from previous Plots.


#This Plot is showing relation between installment and loan amount, this is pretty obvious when 
#loan amount is higher installment would be higher.
Plot14 <- ggplot(charged_off, aes(loan_amnt,installment)) + geom_point()  + 
  geom_smooth()

Plot14


#This Plot is showing relation between total_rec_prncp and total_pymnt amount, here we can see concentration4
#near low value as this is the defaulter data set, less people paid the money

Plot15 <- ggplot(charged_off, aes(total_rec_prncp,total_pymnt)) + geom_point()  + 
  geom_smooth()

Plot15

# END of Analysis.
