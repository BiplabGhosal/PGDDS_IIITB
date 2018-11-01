#Install packages if not installed already.
#install.packages("stringr")
#install.packages("tidyr")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("corrplot")
#install.packages("VIM")
#install.packages("treemap")
#install.packages("cowplot")
#install.packages("Information")
#install.packages("DMwR")
#install.packages("merTools")

library(stringr)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot, quietly = TRUE)
library(VIM)
library(treemap)
library(e1071)
library(cowplot)
library(Information)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
library(GGally)
library(DMwR)
library(merTools)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ROCR)

demographic <- read.csv('Demographic data.csv',stringsAsFactors = FALSE,na.strings = c("NA",'',"Na",' '))
credit_bureau <- read.csv('Credit Bureau data.csv',stringsAsFactors = FALSE,na.strings = c("NA",'',"Na",' '))

#Check the dimensions for demographic and credit bureau data-

dim(demographic) # 71295 x 12
dim(credit_bureau) # 71295 x 19

#Check for unique rows present in the dataframe.

n_distinct(demographic$Application.ID)  #71292.

n_distinct(credit_bureau$Application.ID) #71292


# As we can see there are duplicate application ids which we have to remove.
# from the dataframe before we merge the datasets
# Since number of duplicate values are very less, we are removing them

demographic <- distinct(demographic, Application.ID,.keep_all = TRUE)
credit_bureau <- distinct(credit_bureau, Application.ID,.keep_all = TRUE)

#Merge the two datasets to build a single dataset for further processing based on Application Id.

combined_dataset <- merge(x = demographic, y = credit_bureau, by = "Application.ID")

#Show top 5 rows
head(combined_dataset,5)

#Removing Performance.Tag.x. One col corresponding to Performance tag will suffice
combined_dataset <- combined_dataset[ , !(names(combined_dataset) %in% 'Performance.Tag.x')]

#-------Data Cleaning

#Age variable. Removing all ages which are less than 18

combined_dataset <-
  combined_dataset %>%
  filter(Age>=18)

#Income variable. Removing all records having income = 0 or less than 1

combined_dataset <- 
  combined_dataset %>%
  filter(Income > 0)

sum(is.na(combined_dataset)) #3168
summary(combined_dataset) 
#most NA values corresponding to Avgas.CC.Utilization.in.last.12.months(1051) & Performance.Tag.y (1425)

#Creating a separate df corresponding to NA values of Performance.Tag.y. This dataframe can be used for testing purpose after model building
combined_df_NA <- combined_dataset[which(is.na(combined_dataset$Performance.Tag.y)),]

#Removing NA values corresponding to Performance.Tag.y from base dataset and examining it
combined_dataset <- combined_dataset[!(is.na(combined_dataset$Performance.Tag.y)),]
summary(combined_dataset)
#Avgas.CC.Utilization.in.last.12.months is still having 1016 records corresponding to NA values.
#It accounts to 1% of values. So will just remove the NA values

combined_dataset <- na.omit(combined_dataset)

#######Outlier treatment of numeric variables

str(combined_dataset)

# Plotted Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

#Income Variable
plot_grid(ggplot(combined_dataset, aes(Income))+ geom_histogram(binwidth = 12.5) + ggtitle("Histogram for Income"),
          ggplot(combined_dataset, aes(x="",y=Income))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for Income"), 
          align = "v",ncol = 1)
#No outlier observed for Income

#No.of.months.in.current.residence variable  
plot_grid(ggplot(combined_dataset, aes(No.of.months.in.current.residence))+ geom_histogram(binwidth = 15) + ggtitle("Histogram for No.of.months.in.current.residence"),
          ggplot(combined_dataset, aes(x="",y=No.of.months.in.current.residence))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for No.of.months.in.current.residence"), 
          align = "v",ncol = 1)
#No outlier observed for Number of months in current residence

#No.of.months.in.current.company variable
plot_grid(ggplot(combined_dataset, aes(No.of.months.in.current.company))+ geom_histogram(binwidth = 15) + ggtitle("Histogram for No.of.months.in.current.company"),
          ggplot(combined_dataset, aes(x="",y=No.of.months.in.current.company))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for No.of.months.in.current.company"), 
          align = "v",ncol = 1)
#Outliers observed for number of months in current company

#Avgas.CC.Utilization.in.last.12.months variable
plot_grid(ggplot(combined_dataset, aes(Avgas.CC.Utilization.in.last.12.months))+ geom_histogram(binwidth = 15) + ggtitle("Histogram for Avgas.CC.Utilization.in.last.12.months"),
          ggplot(combined_dataset, aes(x="",y=Avgas.CC.Utilization.in.last.12.months))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for Avgas.CC.Utilization.in.last.12.months"), 
          align = "v",ncol = 1)
#Outliers observed for Avgas.CC.Utilization.in.last.12.months

#Outstanding.Balance variable
plot_grid(ggplot(combined_dataset, aes(Outstanding.Balance))+ geom_histogram(binwidth = 680000) + ggtitle("Histogram for Outstanding.Balance"),
          ggplot(combined_dataset, aes(x="",y=Outstanding.Balance))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for Outstanding.Balance"), 
          align = "v",ncol = 1)
#No outliers observed for Outstanding.Balance

#Total.No.of.Trades variable
plot_grid(ggplot(combined_dataset, aes(Total.No.of.Trades))+ geom_histogram(binwidth = 6) + ggtitle("Histogram for Total.No.of.Trades"),
          ggplot(combined_dataset, aes(x="",y=Total.No.of.Trades))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for Total.No.of.Trades"), 
          align = "v",ncol = 1)
#Outliers observed for Total.No.of.Trades


# There are clear outliers in numeric variables like No.of.months.in.current.company, 
# Avgas.CC.Utilization.in.last.12.months and Total.No.of.Trades

#Outlier treatment and imputing missing value
# The Boxplots showed outliers for No.of.months.in.current.company, 
# Avgas.CC.Utilization.in.last.12.months and Total.No.of.Trades. 
#Here confirming it with percentiles for all the continuous variables
sapply(combined_dataset[,c("Income", "No.of.months.in.current.residence", 
                           "No.of.months.in.current.company", "Avgas.CC.Utilization.in.last.12.months",
                           "Outstanding.Balance","Total.No.of.Trades")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T))

#There is a sudden jump in No.of.months.in.current.company values between 99% and 100%.
#So, all values above 74 (99%) are capped to 74.
combined_dataset$No.of.months.in.current.company[which(combined_dataset$No.of.months.in.current.company>74)] <- 74

#There is a sudden jump in Avgas.CC.Utilization.in.last.12.months values between 94% and 95%.
#So, all values above 91 (94%) are capped to 91.
combined_dataset$Avgas.CC.Utilization.in.last.12.months[which(combined_dataset$Avgas.CC.Utilization.in.last.12.months>91)] <- 91

#There is a sudden jump in Total.No.of.Trades values between 99% and 100%.
#So, all values above 31 (99%) are capped to 31.
combined_dataset$Total.No.of.Trades[which(combined_dataset$Total.No.of.Trades>31)] <- 31

############################################################################

combined_dataset <- mutate(combined_dataset, age_grp = ifelse(Age <=30, 'Young',
                                                              if_else(Age > 30 & Age <= 60, 'MiddleAge'
                                                                      ,'SeniorCitizen')))

combined_dataset <- mutate(combined_dataset, salary_grp = ifelse(Income <=10, 'Low Income',
                                                                 if_else(Income > 10 & Income <= 30, 'Middle Income'
                                                                         ,'High Income')))

# Plotted the Correlation between numeric variables

ggpairs(combined_dataset[, c('Age', 'No.of.dependents', 'Income', 'No.of.months.in.current.residence', 'No.of.months.in.current.company',
                             'No.of.times.90.DPD.or.worse.in.last.6.months', 'No.of.times.60.DPD.or.worse.in.last.6.months',
                             'No.of.times.30.DPD.or.worse.in.last.6.months', 'No.of.times.90.DPD.or.worse.in.last.12.months',
                             'No.of.times.60.DPD.or.worse.in.last.12.months', 'No.of.times.30.DPD.or.worse.in.last.12.months',
                             'Avgas.CC.Utilization.in.last.12.months','No.of.trades.opened.in.last.6.months',
                             'No.of.trades.opened.in.last.12.months', 'No.of.PL.trades.opened.in.last.6.months',
                             'No.of.PL.trades.opened.in.last.12.months', 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.',
                             'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.', 'Outstanding.Balance',
                             'Total.No.of.Trades')])

#Gender vs Performance Tag
Plot1<- ggplot(combined_dataset, aes(factor(Performance.Tag.y), group = Gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Percentage of Performance Tag w.r.t Gender', x='Performance Tag',y='Relative Percentage') + 
  facet_grid(~Gender) +
  scale_fill_discrete(name = "Performance Tag")

Plot1

#Marital Status vs Performance Tag
Plot2<- ggplot(combined_dataset, aes(factor(Performance.Tag.y), group = Marital.Status..at.the.time.of.application.)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Percentage of Performance Tag w.r.t Marital Status', x='Performance Tag',y='Relative Percentage') + 
  facet_grid(~Marital.Status..at.the.time.of.application.) +
  scale_fill_discrete(name = "Performance Tag")

Plot2

#No of dependents vs Performance Tag
Plot3<- ggplot(combined_dataset, aes(factor(Performance.Tag.y), group = No.of.dependents)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Percentage of Performance Tag w.r.t Number of dependents', x='Performance Tag',y='Relative Percentage') + 
  facet_grid(~No.of.dependents) +
  scale_fill_discrete(name = "Performance Tag")

Plot3

#Age Group vs Performance Tag
Plot4<- ggplot(combined_dataset, aes(factor(Performance.Tag.y), group = age_grp)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Percentage of Performance Tag w.r.t Age Group', x='Performance Tag',y='Relative Percentage') + 
  facet_grid(~age_grp) +
  scale_fill_discrete(name = "Performance Tag")

Plot4

#Salary Group vs Performance Tag

Plot5<- ggplot(combined_dataset, aes(factor(Performance.Tag.y), group = salary_grp)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Percentage of Performance Tag w.r.t Salary Group', x='Performance Tag',y='Relative Percentage') + 
  facet_grid(~salary_grp) +
  scale_fill_discrete(name = "Performance Tag")

Plot5

#Education vs Performance Tag
Plot6<- ggplot(combined_dataset, aes(factor(Performance.Tag.y), group = Education)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Percentage of Performance Tag w.r.t Education', x='Performance Tag',y='Relative Percentage') + 
  facet_grid(~Education) +
  scale_fill_discrete(name = "Performance Tag")

Plot6

#Profession vs Performance Tag
Plot7<- ggplot(combined_dataset, aes(factor(Performance.Tag.y), group = Profession)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Percentage of Performance Tag w.r.t Profession', x='Performance Tag',y='Relative Percentage') + 
  facet_grid(~Profession) +
  scale_fill_discrete(name = "Performance Tag")

Plot7

#Residence type vs Performance Tag
Plot8<- ggplot(combined_dataset, aes(factor(Performance.Tag.y), group = Type.of.residence)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Percentage of Performance Tag w.r.t Type of Residence', x='Performance Tag',y='Relative Percentage') + 
  facet_grid(~Type.of.residence) +
  scale_fill_discrete(name = "Performance Tag")

Plot8

#Number of months in current residence vs Performance Tag
Plot9 <- 
  combined_dataset %>%
  dplyr::select(Performance.Tag.y, No.of.months.in.current.residence) %>%
  group_by(Performance.Tag.y) %>%
  summarise(n=mean(No.of.months.in.current.residence, na.rm = TRUE)) %>%
  ggplot(aes(x=factor(Performance.Tag.y),y=n,fill=factor(Performance.Tag.y))) + geom_bar(stat = 'identity')  +
  labs(x='Performance Tag',y='Average months in current residence') + 
  ggtitle('Average number of months in current residence vs Performance Tag') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  scale_fill_discrete(name = "Performance Tag")

Plot9

#Number of months in current company vs Performance Tag
Plot10 <- 
  combined_dataset %>%
  dplyr::select(Performance.Tag.y, No.of.months.in.current.company) %>%
  group_by(Performance.Tag.y) %>%
  summarise(n=mean(No.of.months.in.current.company, na.rm = TRUE)) %>%
  ggplot(aes(x=factor(Performance.Tag.y),y=n,fill=factor(Performance.Tag.y))) + geom_bar(stat = 'identity')  +
  labs(x='Performance Tag',y='Average months in current company') + 
  ggtitle('Average number of months in current company vs Performance Tag') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  scale_fill_discrete(name = "Performance Tag") 

Plot10

# No.of.times.90.DPD.or.worse.in.last.6.months vs Performance Tag
Plot11 <- 
  combined_dataset %>%
  dplyr::select(Performance.Tag.y, No.of.times.90.DPD.or.worse.in.last.6.months) %>%
  group_by(Performance.Tag.y) %>%
  summarise(n=mean(No.of.times.90.DPD.or.worse.in.last.6.months,na.rm=TRUE)) %>%
  ggplot(aes(x=factor(Performance.Tag.y),y=n,fill=factor(Performance.Tag.y))) + geom_bar(stat = 'identity')  +
  labs(x='Performance Tag',y='Mean of 90 DPD or worse in last 6 months') + 
  ggtitle('Mean of 90 DPD or worse in last 6 months vs Performance Tag') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  scale_fill_discrete(name = "Performance Tag") 

Plot11

# No.of.times.60.DPD.or.worse.in.last.6.months vs Performance Tag
Plot12 <- 
  combined_dataset %>%
  dplyr::select(Performance.Tag.y, No.of.times.60.DPD.or.worse.in.last.6.months) %>%
  group_by(Performance.Tag.y) %>%
  summarise(n=mean(No.of.times.60.DPD.or.worse.in.last.6.months)) %>%
  ggplot(aes(x=factor(Performance.Tag.y),y=n,fill=factor(Performance.Tag.y))) + geom_bar(stat = 'identity')  +
  labs(x='Performance Tag',y='Mean of 60 DPD or worse in last 6 months') + 
  ggtitle('Mean of 60 DPD or worse in last 6 months vs Performance Tag') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  scale_fill_discrete(name = "Performance Tag") 

Plot12

# No.of.times.30.DPD.or.worse.in.last.6.months vs Performance Tag
Plot13 <- 
  combined_dataset %>%
  dplyr::select(Performance.Tag.y, No.of.times.30.DPD.or.worse.in.last.6.months) %>%
  group_by(Performance.Tag.y) %>%
  summarise(n=mean(No.of.times.30.DPD.or.worse.in.last.6.months)) %>%
  ggplot(aes(x=factor(Performance.Tag.y),y=n,fill=factor(Performance.Tag.y))) + geom_bar(stat = 'identity')  +
  labs(x='Performance Tag',y='Mean of 30 DPD or worse in last 6 months') + 
  ggtitle('Mean of 30 DPD or worse in last 6 months vs Performance Tag') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  scale_fill_discrete(name = "Performance Tag")

Plot13

#Avgas.CC.Utilization.in.last.12.months vs Peformance Tag
Plot14 <- 
  combined_dataset %>%
  dplyr::select(Performance.Tag.y, Avgas.CC.Utilization.in.last.12.months) %>%
  group_by(Performance.Tag.y) %>%
  summarise(n=mean(Avgas.CC.Utilization.in.last.12.months)) %>%
  ggplot(aes(x=factor(Performance.Tag.y),y=n,fill=factor(Performance.Tag.y))) + geom_bar(stat = 'identity')  +
  labs(x='Performance Tag',y='Percentage of Avg CC utilization') + 
  ggtitle('Mean of Avg CC utilization vs Performance Tag') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  scale_fill_discrete(name = "Performance Tag")

Plot14

#No.of.trades.opened.in.last.6.months vs Performance Tag
Plot15 <- 
  combined_dataset %>%
  dplyr::select(Performance.Tag.y, No.of.trades.opened.in.last.6.months) %>%
  group_by(Performance.Tag.y) %>%
  summarise(n=mean(No.of.trades.opened.in.last.6.months)) %>%
  ggplot(aes(x=factor(Performance.Tag.y),y=n,fill=factor(Performance.Tag.y))) + geom_bar(stat = 'identity')  +
  labs(x='Performance Tag',y='Mean of number of trades opened in last 6 months') + 
  ggtitle('Mean of number of trades opened in last 6 months vs Performance Tag') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  scale_fill_discrete(name = "Performance Tag")

Plot15

#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. vs Performance Tag
Plot16 <- 
  combined_dataset %>%
  dplyr::select(Performance.Tag.y, No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.) %>%
  group_by(Performance.Tag.y) %>%
  summarise(n=mean(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)) %>%
  ggplot(aes(x=factor(Performance.Tag.y),y=n,fill= factor(Performance.Tag.y))) + geom_bar(stat = 'identity')  +
  labs(x='Performance Tag',y='Mean of number of enquires made in last 6 months') + 
  ggtitle('Mean of number of enquiries made in last 6 months vs Performance Tag') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  scale_fill_discrete(name = "Performance Tag")

Plot16

#Presence.of.open.home.loan vs Performance Tag

Plot17<- ggplot(combined_dataset, aes(factor(Performance.Tag.y), group = Presence.of.open.home.loan)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Percentage of Performance Tag w.r.t Open Home Loans', x='Performance Tag',y='Relative Percentage') + 
  facet_grid(~Presence.of.open.home.loan)

Plot17

#Outstanding Balance vs Performance Tag
Plot18 <- 
  combined_dataset %>%
  dplyr::select(Performance.Tag.y, Outstanding.Balance) %>%
  group_by(Performance.Tag.y) %>%
  #summarise(n=sum(Outstanding.Balance)) %>%
  ggplot(aes(x=factor(Performance.Tag.y),y=Outstanding.Balance,fill=factor(Performance.Tag.y))) + geom_bar(stat = 'identity')  +
  labs(x='Performance Tag',y='Mean of Outstanding Balance') + ggtitle('Mean of Outstanding Balance vs Performance Tag') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  scale_fill_discrete(name = "Performance Tag")

Plot18

#Total No of Trades vs Performance Tag
Plot19 <- 
  combined_dataset %>%
  dplyr::select(Performance.Tag.y, Total.No.of.Trades) %>%
  group_by(Performance.Tag.y) %>%
  summarise(n=mean(Total.No.of.Trades)) %>%
  ggplot(aes(x=factor(Performance.Tag.y),y=n,fill=factor(Performance.Tag.y))) + geom_bar(stat = 'identity')  +
  labs(x='Performance Tag',y='Mean of Total.No.of.Trades') + ggtitle('Mean of Total.No.of.Trades vs Performance Tag') +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  scale_fill_discrete(name = "Performance Tag")

Plot19

#Presence.of.open.auto.loan vs Performance Tag
Plot20<- ggplot(combined_dataset, aes(factor(Performance.Tag.y), group = Presence.of.open.auto.loan)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Percentage of Performance Tag w.r.t Open Auto Loans', x='Performance Tag',y='Relative Percentage') + 
  facet_grid(~Presence.of.open.auto.loan)

Plot20

#Changing colummn name of Performance.Tag.y to Performance.Tag
colnames(combined_dataset)[which(names(combined_dataset) == "Performance.Tag.y")] <- "Performance.Tag"

#-------------------------------------------------------------------------------------------------------

#Creating a seperate dataset for IV and WOE calculation
combined_WOE <- combined_dataset

#Removing derived variables from the WOE dataset
combined_WOE <- combined_WOE[ , -which(names(combined_WOE) %in% c("age_grp","salary_grp"))]

summary(combined_WOE)

#Creating bins
combined_WOE$Age <- 
  as.factor(cut(combined_WOE$Age, breaks = c(17, 30, 35, 38, 41, 44, 47, 50, 53, 57, 65)))

combined_WOE$No.of.dependents <- 
  as.factor(cut(combined_WOE$No.of.dependents, breaks = c(0, 1, 2, 3, 4, 5)))

combined_WOE$Income <- 
  as.factor(cut(combined_WOE$Income, breaks = c(0, 5, 10, 16, 21, 26, 31, 36, 41, 48, 60)))

combined_WOE$No.of.months.in.current.residence <- 
  as.factor(cut(combined_WOE$No.of.months.in.current.residence, breaks = c(5, 9, 28, 49, 71, 97, 126)))

combined_WOE$No.of.months.in.current.company <- 
  as.factor(cut(combined_WOE$No.of.months.in.current.company, breaks = c(2, 5, 12, 19, 26, 33, 40, 47, 53, 61, 74)))

combined_WOE$No.of.times.90.DPD.or.worse.in.last.6.months <- 
  as.factor(cut(combined_WOE$No.of.times.90.DPD.or.worse.in.last.6.months, breaks = c(-1, 0, 3)))

combined_WOE$No.of.times.60.DPD.or.worse.in.last.6.months <- 
  as.factor(cut(combined_WOE$No.of.times.60.DPD.or.worse.in.last.6.months, breaks = c(-1, 0, 5)))

combined_WOE$No.of.times.30.DPD.or.worse.in.last.6.months <- 
  as.factor(cut(combined_WOE$No.of.times.30.DPD.or.worse.in.last.6.months, breaks = c(-1, 0, 1, 7)))

combined_WOE$No.of.times.90.DPD.or.worse.in.last.12.months <- 
  as.factor(cut(combined_WOE$No.of.times.90.DPD.or.worse.in.last.12.months, breaks = c(-1, 0, 1, 5)))

combined_WOE$No.of.times.60.DPD.or.worse.in.last.12.months <- 
  as.factor(cut(combined_WOE$No.of.times.60.DPD.or.worse.in.last.12.months, breaks = c(-1, 0, 1, 7)))

combined_WOE$No.of.times.30.DPD.or.worse.in.last.12.months <- 
  as.factor(cut(combined_WOE$No.of.times.30.DPD.or.worse.in.last.12.months, breaks = c(-1, 0, 2, 9)))

combined_WOE$Avgas.CC.Utilization.in.last.12.months <- 
  as.factor(cut(combined_WOE$Avgas.CC.Utilization.in.last.12.months, breaks = c(-1, 0, 4, 6, 8, 11, 14, 21, 37, 51, 71, 91)))

combined_WOE$No.of.trades.opened.in.last.6.months <- 
  as.factor(cut(combined_WOE$No.of.trades.opened.in.last.6.months, breaks = c(-1, 0, 1, 2, 3, 4, 12)))

combined_WOE$No.of.trades.opened.in.last.12.months <- 
  as.factor(cut(combined_WOE$No.of.trades.opened.in.last.12.months, breaks = c(-1, 0, 1, 2, 4, 5, 7, 12, 28)))

combined_WOE$No.of.PL.trades.opened.in.last.6.months <- 
  as.factor(cut(combined_WOE$No.of.PL.trades.opened.in.last.6.months, breaks = c(-1, 0, 1, 2, 6)))

combined_WOE$No.of.PL.trades.opened.in.last.12.months <- 
  as.factor(cut(combined_WOE$No.of.PL.trades.opened.in.last.12.months, breaks = c(-1, 0, 1, 2, 3, 4, 5, 12)))

combined_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 
  as.factor(cut(combined_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., breaks = c(-1, 0, 1, 2, 4, 10)))

combined_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 
  as.factor(cut(combined_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., breaks = c(-1, 0, 1, 2, 3, 4, 5, 8, 20)))

combined_WOE$Presence.of.open.home.loan <- 
  as.factor(cut(combined_WOE$Presence.of.open.home.loan, breaks = c(-1, 0, 1)))

combined_WOE$Outstanding.Balance <- 
  as.factor(cut(combined_WOE$Outstanding.Balance, breaks = c(-1, 0, 7792, 60287, 393081, 590356, 777830, 975881, 1362097, 2961784, 3290266, 5218801)))

combined_WOE$Total.No.of.Trades <- 
  as.factor(cut(combined_WOE$Total.No.of.Trades, breaks = c(0, 1, 2, 3, 4, 5, 7, 8, 10, 19, 31)))

combined_WOE$Presence.of.open.auto.loan <- 
  as.factor(cut(combined_WOE$Presence.of.open.auto.loan, breaks = c(-1, 0, 1)))

#Computing IV and WOE on combined_WOE dataset
#So for WOE dataset, 0 represents defaulted customers and 1 represents non-defaulted customers
IV <- create_infotables(data = combined_WOE, y='Performance.Tag',bins = 10, parallel = FALSE)
IV_value <- data.frame(IV$Summary)

#Based on the IV value, we can see that some of the strong predictiors of dependent variable are:
#(Considering IV value greater than 0.20 as strong predicator variables:

#1 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.3170061
#2                          Avgas.CC.Utilization.in.last.12.months 0.3141297
#3                        No.of.PL.trades.opened.in.last.12.months 0.3122692
#4                           No.of.trades.opened.in.last.12.months 0.3118740
#5                                             Outstanding.Balance 0.2569574
#6                                              Total.No.of.Trades 0.2511587
#7                    No.of.times.30.DPD.or.worse.in.last.6.months 0.2469250
#8                         No.of.PL.trades.opened.in.last.6.months 0.2286191
#9                   No.of.times.90.DPD.or.worse.in.last.12.months 0.2184706
#10 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 0.2144031
#11                   No.of.times.60.DPD.or.worse.in.last.6.months 0.2101906
#12                  No.of.times.30.DPD.or.worse.in.last.12.months 0.2026160

#Plotting the variables w.r.t IV value
MultiPlot(IV,IV$Summary$Variable[1:9],same_scale=TRUE)
MultiPlot(IV,IV$Summary$Variable[10:18],same_scale=TRUE)
MultiPlot(IV,IV$Summary$Variable[19:27],same_scale=TRUE)
MultiPlot(IV,IV$Summary$Variable[28],same_scale=FALSE)

#Plotting the Age variable w.r.t WOE value
plot_infotables(IV,"Age")
#The customers in the Age group (50,53] are very likely to default, since the WOE is quite low

#Plotting the Gender variable w.r.t WOE value
plot_infotables(IV,"Gender")
#Male customers are more prone to default than Female, since the WOE is low

#Plotting the Marital.Status..at.the.time.of.application. variable w.r.t WOE value
plot_infotables(IV,"Marital.Status..at.the.time.of.application.")

#Plotting the No.of.dependents variable w.r.t WOE value  
plot_infotables(IV,"No.of.dependents")

#Plotting the Income variable w.r.t WOE value
plot_infotables(IV,"Income")

#Plotting the Education variable w.r.t WOE value
plot_infotables(IV,"Education")

#Plotting the Profession variable w.r.t WOE value
plot_infotables(IV,"Profession")

#Plotting the Type.of.residence variable w.r.t WOE value
plot_infotables(IV,"Type.of.residence")

#Plotting the No.of.months.in.current.residence variable w.r.t WOE value
plot_infotables(IV,"No.of.months.in.current.residence")

#Plotting the No.of.months.in.current.company variable w.r.t WOE value
plot_infotables(IV,"No.of.months.in.current.company")

#Plotting the No.of.times.90.DPD.or.worse.in.last.6.months variable w.r.t WOE value
plot_infotables(IV,"No.of.times.90.DPD.or.worse.in.last.6.months")

#Plotting the No.of.times.60.DPD.or.worse.in.last.6.months variable w.r.t WOE value
plot_infotables(IV,"No.of.times.60.DPD.or.worse.in.last.6.months")

#Plotting the No.of.times.30.DPD.or.worse.in.last.6.months variable w.r.t WOE value
plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.6.months")

#Plotting the No.of.times.90.DPD.or.worse.in.last.12.months variable w.r.t WOE value
plot_infotables(IV,"No.of.times.90.DPD.or.worse.in.last.12.months")

#Plotting the No.of.times.60.DPD.or.worse.in.last.12.months variable w.r.t WOE value
plot_infotables(IV,"No.of.times.60.DPD.or.worse.in.last.12.months")

#Plotting the No.of.times.30.DPD.or.worse.in.last.12.months variable w.r.t WOE value
plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.12.months")

#Plotting the Avgas.CC.Utilization.in.last.12.months variable w.r.t WOE value
plot_infotables(IV,"Avgas.CC.Utilization.in.last.12.months")

#Plotting the No.of.trades.opened.in.last.6.months variable w.r.t WOE value
plot_infotables(IV,"No.of.trades.opened.in.last.6.months")

#Plotting the No.of.trades.opened.in.last.12.months variable w.r.t WOE value
plot_infotables(IV,"No.of.trades.opened.in.last.12.months")

#Plotting the No.of.PL.trades.opened.in.last.6.months variable w.r.t WOE value
plot_infotables(IV,"No.of.PL.trades.opened.in.last.6.months")

#Plotting the No.of.PL.trades.opened.in.last.12.months variable w.r.t WOE value
plot_infotables(IV,"No.of.PL.trades.opened.in.last.12.months")

#Plotting the No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. variable w.r.t WOE value
plot_infotables(IV,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")

#Plotting the No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. variable w.r.t WOE value
plot_infotables(IV,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")
#The customers who haven't enquired in the last 12 months are very likely to default

#Plotting the Presence.of.open.home.loan variable w.r.t WOE value
plot_infotables(IV,"Presence.of.open.home.loan")

#Plotting the Outstanding.Balance variable w.r.t WOE value
plot_infotables(IV,"Outstanding.Balance")

#Plotting the Total.No.of.Trades variable w.r.t WOE value
plot_infotables(IV,"Total.No.of.Trades")

#Plotting the Presence.of.open.auto.loan variable w.r.t WOE value
plot_infotables(IV,"Presence.of.open.auto.loan")

#Replacing the actual values of the variables with corresponding WOE values
combined_WOE$Age <- mapvalues(combined_WOE$Age, from = IV$Tables$Age$Age, to = IV$Tables$Age$WOE)
combined_WOE$Gender <- mapvalues(combined_WOE$Gender, from = IV$Tables$Gender$Gender, to = IV$Tables$Gender$WOE)
combined_WOE$Marital.Status..at.the.time.of.application. <- mapvalues(combined_WOE$Marital.Status..at.the.time.of.application., from = IV$Tables$Marital.Status..at.the.time.of.application.$Marital.Status..at.the.time.of.application., to = IV$Tables$Marital.Status..at.the.time.of.application.$WOE)
combined_WOE$No.of.dependents <- mapvalues(combined_WOE$No.of.dependents, from = IV$Tables$No.of.dependents$No.of.dependents, to = IV$Tables$No.of.dependents$WOE)
combined_WOE$Income <- mapvalues(combined_WOE$Income, from = IV$Tables$Income$Income, to = IV$Tables$Income$WOE)
combined_WOE$Education <- mapvalues(combined_WOE$Education, from = IV$Tables$Education$Education, to = IV$Tables$Education$WOE)
combined_WOE$Profession <- mapvalues(combined_WOE$Profession, from = IV$Tables$Profession$Profession, to = IV$Tables$Profession$WOE)
combined_WOE$Type.of.residence <- mapvalues(combined_WOE$Type.of.residence, from = IV$Tables$Type.of.residence$Type.of.residence, to = IV$Tables$Type.of.residence$WOE)
combined_WOE$No.of.months.in.current.residence <- mapvalues(combined_WOE$No.of.months.in.current.residence, from = IV$Tables$No.of.months.in.current.residence$No.of.months.in.current.residence, to = IV$Tables$No.of.months.in.current.residence$WOE)
combined_WOE$No.of.months.in.current.company <- mapvalues(combined_WOE$No.of.months.in.current.company, from = IV$Tables$No.of.months.in.current.company$No.of.months.in.current.company, to = IV$Tables$No.of.months.in.current.company$WOE)
combined_WOE$No.of.times.90.DPD.or.worse.in.last.6.months <- mapvalues(combined_WOE$No.of.times.90.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$No.of.times.90.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$WOE)
combined_WOE$No.of.times.60.DPD.or.worse.in.last.6.months <- mapvalues(combined_WOE$No.of.times.60.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$No.of.times.60.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$WOE)
combined_WOE$No.of.times.30.DPD.or.worse.in.last.6.months <- mapvalues(combined_WOE$No.of.times.30.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$No.of.times.30.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$WOE)
combined_WOE$No.of.times.90.DPD.or.worse.in.last.12.months <- mapvalues(combined_WOE$No.of.times.90.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$No.of.times.90.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$WOE)
combined_WOE$No.of.times.60.DPD.or.worse.in.last.12.months <- mapvalues(combined_WOE$No.of.times.60.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$No.of.times.60.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$WOE)
combined_WOE$No.of.times.30.DPD.or.worse.in.last.12.months <- mapvalues(combined_WOE$No.of.times.30.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$No.of.times.30.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$WOE)
combined_WOE$Avgas.CC.Utilization.in.last.12.months <- mapvalues(combined_WOE$Avgas.CC.Utilization.in.last.12.months, from = IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months, to = IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE)
combined_WOE$No.of.trades.opened.in.last.6.months <- mapvalues(combined_WOE$No.of.trades.opened.in.last.6.months, from = IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months, to = IV$Tables$No.of.trades.opened.in.last.6.months$WOE)
combined_WOE$No.of.trades.opened.in.last.12.months <- mapvalues(combined_WOE$No.of.trades.opened.in.last.12.months, from = IV$Tables$No.of.trades.opened.in.last.12.months$No.of.trades.opened.in.last.12.months, to = IV$Tables$No.of.trades.opened.in.last.12.months$WOE)
combined_WOE$No.of.PL.trades.opened.in.last.6.months <- mapvalues(combined_WOE$No.of.PL.trades.opened.in.last.6.months, from = IV$Tables$No.of.PL.trades.opened.in.last.6.months$No.of.PL.trades.opened.in.last.6.months, to = IV$Tables$No.of.PL.trades.opened.in.last.6.months$WOE)
combined_WOE$No.of.PL.trades.opened.in.last.12.months <- mapvalues(combined_WOE$No.of.PL.trades.opened.in.last.12.months, from = IV$Tables$No.of.PL.trades.opened.in.last.12.months$No.of.PL.trades.opened.in.last.12.months, to = IV$Tables$No.of.PL.trades.opened.in.last.12.months$WOE)
combined_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- mapvalues(combined_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$WOE)
combined_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- mapvalues(combined_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$WOE)
combined_WOE$Presence.of.open.home.loan <- mapvalues(combined_WOE$Presence.of.open.home.loan, from = IV$Tables$Presence.of.open.home.loan$Presence.of.open.home.loan, to = IV$Tables$Presence.of.open.home.loan$WOE)
combined_WOE$Outstanding.Balance <- mapvalues(combined_WOE$Outstanding.Balance, from = IV$Tables$Outstanding.Balance$Outstanding.Balance, to = IV$Tables$Outstanding.Balance$WOE)
combined_WOE$Total.No.of.Trades <- mapvalues(combined_WOE$Total.No.of.Trades, from = IV$Tables$Total.No.of.Trades$Total.No.of.Trades, to = IV$Tables$Total.No.of.Trades$WOE)
combined_WOE$Presence.of.open.auto.loan <- mapvalues(combined_WOE$Presence.of.open.auto.loan, from = IV$Tables$Presence.of.open.auto.loan$Presence.of.open.auto.loan, to = IV$Tables$Presence.of.open.auto.loan$WOE)

#Since from the IV values we have found the medium and strong predictor variables
#We will use only those variables for our combined_WOE dataset
combined_WOE <- combined_WOE[ , which(names(combined_WOE) %in% 
                                        c("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
                                          "Avgas.CC.Utilization.in.last.12.months",
                                          "No.of.PL.trades.opened.in.last.12.months",
                                          "No.of.trades.opened.in.last.12.months",
                                          "Outstanding.Balance","Total.No.of.Trades",
                                          "No.of.times.30.DPD.or.worse.in.last.6.months",
                                          "No.of.PL.trades.opened.in.last.6.months",
                                          "No.of.times.90.DPD.or.worse.in.last.12.months",
                                          "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                                          "No.of.times.60.DPD.or.worse.in.last.6.months",
                                          "No.of.times.30.DPD.or.worse.in.last.12.months",
                                          "No.of.trades.opened.in.last.6.months",
                                          "No.of.times.60.DPD.or.worse.in.last.12.months",
                                          "No.of.times.90.DPD.or.worse.in.last.6.months",
                                          "Performance.Tag"))]

#----------------------------------------------------------------------------------------
############ MODEL-1 ############## LOGISTIC REGRESSION ON COMBINED DATASET WITHOUT SMOTE #############

#Logistic Regression Model Building on the combined dataset using the unbalanced data(without SMOTE)

str(combined_dataset)

#Removing the derived columns as they will result in multicollinearity
combined_dataset <- combined_dataset[ , -which(names(combined_dataset) %in% c("age_grp","salary_grp"))]

# Taking a backup
combined_dataset_No_WOE <- combined_dataset
str(combined_dataset_No_WOE)

#Converting from character to factor variables and dummy variable creation
combined_dataset_No_WOE$Gender <- as.factor(combined_dataset_No_WOE$Gender)
combined_dataset_No_WOE$Marital.Status..at.the.time.of.application. <- as.factor(combined_dataset_No_WOE$Marital.Status..at.the.time.of.application.)
combined_dataset_No_WOE$Education <- as.factor(combined_dataset$Education)
combined_dataset_No_WOE$Profession <- as.factor(combined_dataset_No_WOE$Profession)
combined_dataset_No_WOE$Type.of.residence <- as.factor(combined_dataset_No_WOE$Type.of.residence)
combined_dataset_No_WOE$Presence.of.open.auto.loan <- as.factor(combined_dataset_No_WOE$Presence.of.open.auto.loan)
combined_dataset_No_WOE$Presence.of.open.home.loan <- as.factor(combined_dataset_No_WOE$Presence.of.open.home.loan)
combined_dataset_No_WOE$Performance.Tag <- as.factor(combined_dataset_No_WOE$Performance.Tag)

factor_variables <- combined_dataset_No_WOE[ , which(names(combined_dataset_No_WOE) %in% c("Gender","Marital.Status..at.the.time.of.application.","Education","Profession","Type.of.residence","Presence.of.open.auto.loan","Presence.of.open.home.loan"))]
dummies<- data.frame(sapply(factor_variables,function(x) data.frame(model.matrix(~x-1,data =factor_variables))[,-1]))
combined_dataset_No_WOE <- cbind(dummies,combined_dataset_No_WOE[ , -which(names(combined_dataset_No_WOE) %in% c("Gender","Marital.Status..at.the.time.of.application.","Education","Profession","Type.of.residence","Presence.of.open.auto.loan","Presence.of.open.home.loan"))])

#Removing ID column
combined_dataset_No_WOE <- combined_dataset_No_WOE[,-which(names(combined_dataset_No_WOE) %in% c("Application.ID"))]

#Normalizing the numeric variables
combined_dataset_No_WOE$Age<- scale(combined_dataset_No_WOE$Age)
combined_dataset_No_WOE$No.of.dependents <- scale(combined_dataset_No_WOE$No.of.dependents)
combined_dataset_No_WOE$Income <- scale(combined_dataset_No_WOE$Income)
combined_dataset_No_WOE$No.of.months.in.current.residence <- scale(combined_dataset_No_WOE$No.of.months.in.current.residence)
combined_dataset_No_WOE$No.of.months.in.current.company <- scale(combined_dataset_No_WOE$No.of.months.in.current.company)
combined_dataset_No_WOE$No.of.times.90.DPD.or.worse.in.last.6.months <- scale(combined_dataset_No_WOE$No.of.times.90.DPD.or.worse.in.last.6.months)
combined_dataset_No_WOE$No.of.times.60.DPD.or.worse.in.last.6.months <- scale(combined_dataset_No_WOE$No.of.times.60.DPD.or.worse.in.last.6.months)
combined_dataset_No_WOE$No.of.times.30.DPD.or.worse.in.last.6.months <- scale(combined_dataset_No_WOE$No.of.times.30.DPD.or.worse.in.last.6.months)
combined_dataset_No_WOE$No.of.times.90.DPD.or.worse.in.last.12.months <- scale(combined_dataset_No_WOE$No.of.times.90.DPD.or.worse.in.last.12.months)
combined_dataset_No_WOE$No.of.times.60.DPD.or.worse.in.last.12.months <- scale(combined_dataset_No_WOE$No.of.times.60.DPD.or.worse.in.last.12.months)
combined_dataset_No_WOE$No.of.times.30.DPD.or.worse.in.last.12.months <- scale(combined_dataset_No_WOE$No.of.times.30.DPD.or.worse.in.last.12.months)
combined_dataset_No_WOE$Avgas.CC.Utilization.in.last.12.months <- scale(combined_dataset_No_WOE$Avgas.CC.Utilization.in.last.12.months)
combined_dataset_No_WOE$No.of.trades.opened.in.last.6.months <- scale(combined_dataset_No_WOE$No.of.trades.opened.in.last.6.months)
combined_dataset_No_WOE$No.of.trades.opened.in.last.12.months <- scale(combined_dataset_No_WOE$No.of.trades.opened.in.last.12.months)
combined_dataset_No_WOE$No.of.PL.trades.opened.in.last.6.months <- scale(combined_dataset_No_WOE$No.of.PL.trades.opened.in.last.6.months)
combined_dataset_No_WOE$No.of.PL.trades.opened.in.last.12.months <- scale(combined_dataset_No_WOE$No.of.PL.trades.opened.in.last.12.months)
combined_dataset_No_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<- scale(combined_dataset_No_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
combined_dataset_No_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- scale(combined_dataset_No_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
combined_dataset_No_WOE$Outstanding.Balance <- scale(combined_dataset_No_WOE$Outstanding.Balance)
combined_dataset_No_WOE$Total.No.of.Trades <- scale(combined_dataset_No_WOE$Total.No.of.Trades)


#Splitting into train and test data
set.seed(1)
split_indices <- sample.split(combined_dataset_No_WOE$Performance.Tag, SplitRatio = 0.70)

train_No_WOE <- combined_dataset_No_WOE[split_indices, ]
test_No_WOE <- combined_dataset_No_WOE[!split_indices, ]

#Initial Model 
logistic_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_No_WOE) 
summary(logistic_1) #AIC: 16180

#StepAIC run
logistic_2 <- stepAIC(logistic_1, direction = "both")
summary(logistic_2) #AIC: 16145

vif(logistic_2)
#Removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans due to very less significance value and high vif value
logistic_3 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.company + 
                    No.of.times.60.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.6.months + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                    Total.No.of.Trades, family = "binomial", data = train_No_WOE)

summary(logistic_3) #AIC: 16146

vif(logistic_3)  

#Removing No.of.times.60.DPD.or.worse.in.last.6.months due to high vif

logistic_4 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.company + 
                    No.of.times.30.DPD.or.worse.in.last.6.months + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                    Total.No.of.Trades, family = "binomial", data = train_No_WOE)

summary(logistic_4) #AIC: 16154

vif(logistic_4)  

#Removing No.of.times.90.DPD.or.worse.in.last.12.months due to lowest significance level and high vif
logistic_5 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.company + 
                    No.of.times.30.DPD.or.worse.in.last.6.months + 
                    Avgas.CC.Utilization.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                    Total.No.of.Trades, family = "binomial", data = train_No_WOE)

summary(logistic_5) #AIC: 16154

vif(logistic_5)    

#Removing No.of.PL.trades.opened.in.last.6.months less significant and high vif
logistic_6 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.company + 
                    No.of.times.30.DPD.or.worse.in.last.6.months + 
                    Avgas.CC.Utilization.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                    Total.No.of.Trades, family = "binomial", data = train_No_WOE)

summary(logistic_6) #AIC: 16156

vif(logistic_6)     

#Removing Total.No.of.Trades , high vif and low significance level
logistic_7 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.company + 
                    No.of.times.30.DPD.or.worse.in.last.6.months + 
                    Avgas.CC.Utilization.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
                  , family = "binomial", data = train_No_WOE)

summary(logistic_7) #AIC: 16158

vif(logistic_7)   

# Creating a temp model with 6 significant variables in the model

temp_model_1<- logistic_7

#----------

### Model Evaluation

### Test Data ####

#predicted probabilities of Performance for test data

test_pred = predict(temp_model_1, type = "response", 
                    newdata = test_No_WOE[,-35])


# Let's see the summary 

summary(test_pred)

# Add the prediction probability with the test data set for further model evaluation steps.
test_No_WOE$prob <- test_pred

# View the test dataset including prediction probabity.
View(test_No_WOE)

# Let's use the probability cutoff of 50%.

test_pred_performance <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_No_WOE$Performance.Tag==1,"Yes","No"))


table(test_actual_performance,test_pred_performance)
#test_pred_performance
#test_actual_performance    No
#No  19693
#Yes   867

test_conf <- confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

#Accuracy : 95%
#Sensitivity : 0%       
#Specificity : 100%

# Here we can see accuaracy of the model is on higher side (95%) but sensitivity is very less.
# As per business scenario we have to predict Performance rate hence Sensitivity need to be on
# on the higher side.

# Let's find out the optimal probalility cutoff .

# Following is the reusable fuction which will be used to get performance based on cutoff
# value iteratively and return sensitivity, specificity and accuracy of the model.

perform_fn <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_performance, test_actual_performance, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

# create 100s of sequence number between 0.01 and 0.80, which will be used as cut-off value
# in each iteration to find out optimal value.
s = seq(.01,.80,length=100)

# Initialize a 100x3 matrix with 0 as default value.
OUT = matrix(0,100,3)

# Call the perform_fn in a loop and assign the output to OUT matrix for "sensitivity", "specificity", "accuracy".
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) +
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  lines(s,OUT[,2],col="darkgreen",lwd=2) +
  lines(s,OUT[,3],col=4,lwd=2) 


# Add legends to the plot created earlier.

legend(0.60,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy")) 

# Add a box over the plot.
box()

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.12)]


# Let's choose a cutoff value of 0.0498 for final model

test_cutoff_performance <- factor(ifelse(test_pred >=0.0498, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

# Get Accuracy, Sensitivity and Specificity from the confusion matrix using 
# cut-off value - 0.1776.
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

# Show Accuracy
acc
# Accuracy : 0.6803

# Show Sensitivity
sens
# Sensitivity : 0.5743

# Show Specificity
spec
# Specificity : 0.6850

#So on Logistic Regression model on the unbalanced combined dataset, we are getting the 
# Accuracy : 0.6803
# Sensitivity : 0.5743
#Specificity : 0.6850

#Since the accuracy parameters are not high, we will try other classification models.
##################################################################################################

############ MODEL-2 ############## LOGISTIC REGRESSION ON DEMOGRAPHIC DATASET WITHOUT SMOTE #############

demographic_cleaned <- combined_dataset %>%
  dplyr::select(names(demographic[,-12]),Performance.Tag)

# Bringing the variables in the correct format
#Gender
demographic_cleaned$Gender <- as.factor(demographic_cleaned$Gender)

#Marital.Status..at.the.time.of.application.
demographic_cleaned$Marital.Status..at.the.time.of.application. <- as.factor(demographic_cleaned$Marital.Status..at.the.time.of.application.)

#Education
demographic_cleaned$Education <- as.factor(demographic_cleaned$Education)

#Profession
demographic_cleaned$Profession <- as.factor(demographic_cleaned$Profession)

#Type.of.residence
demographic_cleaned$Type.of.residence <- as.factor(demographic_cleaned$Type.of.residence)

#Performance.Tag
demographic_cleaned$Performance.Tag <- as.factor(demographic_cleaned$Performance.Tag)

# Normalising continuous variables 
demographic_cleaned$Age<- scale(demographic_cleaned$Age)
demographic_cleaned$No.of.dependents<- scale(demographic_cleaned$No.of.dependents)
demographic_cleaned$Income<- scale(demographic_cleaned$Income)
demographic_cleaned$No.of.months.in.current.residence<- scale(demographic_cleaned$No.of.months.in.current.residence)
demographic_cleaned$No.of.months.in.current.company<- scale(demographic_cleaned$No.of.months.in.current.company)

# Subsetting the categorical variables to a dataframe
demographic_cleaned_Chr<- 
  demographic_cleaned[,c('Gender','Marital.Status..at.the.time.of.application.','Education'
                         ,'Profession', 'Type.of.residence')]

# Converting the categorical variables to factor
demographic_cleaned_fact<- data.frame(sapply(demographic_cleaned_Chr, function(x) factor(x)))

# Creating dummy variables for categorical columns
dummies<- data.frame(sapply(demographic_cleaned_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =demographic_cleaned_fact))[,-1]))

# The Final dataset
demographic_cleaned_final<- cbind(demographic_cleaned[,-c(1,3,4,7,8,9)],dummies) 
View(demographic_cleaned_final) #4300 variables of 52 variables

#Splitting the data to create Train and Test data
set.seed(100)

indices <- sample.split(demographic_cleaned_final$'Performance.Tag', SplitRatio = 0.7)

train <- demographic_cleaned_final[indices,]
test <- demographic_cleaned_final[!(indices), ]

#Model Creation

#Initial model
model_1 <- glm(Performance.Tag ~ ., data = train, family = "binomial")
summary(model_1) #AIC: 16703

#Step-wise selection
model_2 <- stepAIC(model_1, direction = "both")
summary(model_2) #AIC: 16686

vif(model_2)

#Removing Education.xPhd as this variable is less significant
model_3 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + No.of.months.in.current.company +
                 Education.xOthers  +Profession.xSE, data = train, family = 'binomial')
summary(model_3) #AIC : 16686
vif(model_3)

#Removing Education.xOthers as this variable is having comparatively high p value  
model_4 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + No.of.months.in.current.company +
                 Profession.xSE, data = train, family = 'binomial')
summary(model_4) #AIC : 16687
vif(model_3)

#Removing Profession.xSE as this variable is having comparatively high p value
model_4 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + No.of.months.in.current.company,
               data = train, family = 'binomial')
summary(model_4) #AIC : 16690

#With 3 significant variables in the model
temp_demogrpahic_model1 <- model_4


### Model Evaluation

#predicted probabilities of Performance for test data
test_pred = predict(temp_demogrpahic_model1, type = "response", 
                    newdata = test[,-6])  

# Let's see the summary 
summary(test_pred)

# Add the prediction probability with the test data set for further model evaluation steps.
test$prob <- test_pred

# View the test dataset including prediction probabity.
View(test)

# Let's use the probability cutoff of 50%.
test_pred_performance <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_performance <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

table(test_actual_performance,test_pred_performance)
#                       test_pred_performance
#test_actual_performance    No
#No  19693
#Yes   867

test_conf <- confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

#Accuracy : 95%
#Sensitivity : 0%       
#Specificity : 100%

# Here we can see accuaracy of the model is on higher side (95%) but sensitivity is very less.
# As per business scenario we have to predict Performance rate hence Sensitivity need to be on
# on the higher side.  

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

# create 100s of sequence number between 0.01 and 0.80, which will be used as cut-off value
# in each iteration to find out optimal value.
s = seq(.01,.80,length=100)

# Initialize a 100x3 matrix with 0 as default value.
OUT = matrix(0,100,3)

# Call the perform_fn in a loop and assign the output toOUT matrix for "sensitivity", "specificity", "accuracy".
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) +
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  lines(s,OUT[,2],col="darkgreen",lwd=2) +
  lines(s,OUT[,3],col=4,lwd=2) 

# Add legends to the plot created earlier.

legend(0.60,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy")) 

# Add a box over the plot.
box()

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.12)]


# Let's choose a cutoff value of 0.0419 for final model

test_cutoff_performance <- factor(ifelse(test_pred >=0.0419, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

# Get Accuracy, Sensitivity and Specificity from the confusion matrix using 

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

# Show Accuracy
acc
# Accuracy : 0.522
# Show Sensitivity
sens
# Sensitivity : 0.597
# Show Specificity
spec
# Specificity : 0.5196

#Since the accuracy parameters are not high, we will the model by balancing the dataset using SMOTE

############ MODEL-3 ############## LOGISTIC REGRESSION ON DEMOGRAPHIC DATASET WITH SMOTE #############

demo_smote_train <- train
table(demo_smote_train$Performance.Tag)
#Clearly the dataset is unbalanced with number of 0's being much more than number of 1's

train_demo_SMOTE <- SMOTE(Performance.Tag ~ ., demo_smote_train, perc.over = 100,perc.under=200)
table(train_demo_SMOTE$Performance.Tag)
#Now the dataset is evenly balanced with equal number of 0 and 1

#Model Building
logistic_1_Demo_SMOTE <- glm(Performance.Tag ~ ., family = "binomial", data = train_demo_SMOTE) 
summary(logistic_1_Demo_SMOTE)   #AIC: 11146
vif(logistic_1_Demo_SMOTE)

# Using stepwise algorithm for removing insignificant variables   
logistic_2_Demo_SMOTE <- stepAIC(logistic_1_Demo_SMOTE, direction = "both")
summary(logistic_2_Demo_SMOTE) #AIC: 11126
vif(logistic_2_Demo_SMOTE)

# Removing Profession.xSE as this variable is not significant
logistic_3_Demo_SMOTE <- glm(Performance.Tag ~ Income+No.of.months.in.current.company+Education.xPhd+
                               Type.of.residence.xOthers, data = train_demo_SMOTE, family = 'binomial')
summary(logistic_3_Demo_SMOTE) #AIC: 11126

# Removing Type.of.residence.xOthers as this variable is not significant
logistic_4_Demo_SMOTE <- glm(Performance.Tag ~ Income+No.of.months.in.current.company+Education.xPhd
                             , data = train_demo_SMOTE, family = 'binomial')
summary(logistic_4_Demo_SMOTE) #AIC: 11128

# Removing Education.xPhd as this variable is not significant
logistic_5_Demo_SMOTE <- glm(Performance.Tag ~ Income+No.of.months.in.current.company
                             , data = train_demo_SMOTE, family = 'binomial')
summary(logistic_5_Demo_SMOTE) #AIC: 11129

#With 2 significant variables in the model
temp_demogrpahic_SMOTE_log_model1 <- logistic_5_Demo_SMOTE

### Model Evaluation

test_demo_SMOTE <- test
#predicted probabilities of Performance for test data
test_pred = predict(temp_demogrpahic_SMOTE_log_model1, type = "response", 
                    newdata = test_demo_SMOTE[,-6])  

# Let's see the summary 
summary(test_pred)

#Removing prob column from test_demo_SMOTE. This column refers to the probabilty of logistic model without SMOTE
test_demo_SMOTE <- test_demo_SMOTE[ , -which(names(test_demo_SMOTE) %in% c("prob"))]

# Add the prediction probability with the test data set for further model evaluation steps.
test_demo_SMOTE$prob <- test_pred

# View the test dataset including prediction probabity.
View(test_demo_SMOTE)

# Let's use the probability cutoff of 50%.
test_pred_performance <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_demo_SMOTE$Performance.Tag==1,"Yes","No"))

table(test_actual_performance,test_pred_performance)
#test_pred_performance
#test_actual_performance    No   Yes
#No  10366  9327
#Yes   350   517

test_conf <- confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

#Accuracy : 53%
#Sensitivity : 59%       
#Specificity : 52%

# Here we can see accuaracy, sensitivity and specificity of the model is too low.

# Creating cutoff values from 0.33 to 0.62 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

# create 100s of sequence number between 0.33 and 0.62, which will be used as cut-off value
# in each iteration to find out optimal value.
s = seq(.33,.62,length=100)

# Initialize a 100x3 matrix with 0 as default value.
OUT = matrix(0,100,3)

# Call the perform_fn in a loop and assign the output toOUT matrix for "sensitivity", "specificity", "accuracy".
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) +
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  lines(s,OUT[,2],col="darkgreen",lwd=2) +
  lines(s,OUT[,3],col=4,lwd=2) 

# Add legends to the plot created earlier.
legend(0.60,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy")) 

# Add a box over the plot.
box()

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1)]

# Let's choose a cutoff value of 0.51 for final model

test_cutoff_performance <- factor(ifelse(test_pred >=0.51, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

# Get Accuracy, Sensitivity and Specificity from the confusion matrix using 

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

# Show Accuracy
acc
# Accuracy : 0.58
# Show Sensitivity
sens
# Sensitivity : 0.52
# Show Specificity
spec
# Specificity : 0.58

#Since the accuracy parameters are not high, we will the model by balancing the dataset using SMOTE


############ MODEL-4 ############## LOGISTIC REGRESSION ON MERGED DATASET WITH SMOTE #############

combined_SMOTE_log_train <- train_No_WOE #Taking a backup

table(combined_SMOTE_log_train$Performance.Tag)
#0     1 
#45952  2024 
#Clearly the dataset is not balanced because number of 0 is more than number of 1.

train_merged_SMOTE <- SMOTE(Performance.Tag ~ ., combined_SMOTE_log_train, perc.over = 100,k=5,perc.under=200)
table(train_merged_SMOTE$Performance.Tag)

#0    1 
#4048 4048 
# The dataset is now balanced with equal number of 0 and 1.
# Variable data are getting distorted. Need to confirm with mentor

#Model Building
logistic_1_Merged_SMOTE <- glm(Performance.Tag ~ ., family = "binomial", data = train_merged_SMOTE) 
summary(logistic_1_Merged_SMOTE)   #AIC: 10523
vif(logistic_1_Merged_SMOTE)

# Using stepwise algorithm for removing insignificant variables   
logistic_2_Merged_SMOTE <- stepAIC(logistic_1_Merged_SMOTE, direction = "both")
summary(logistic_2_Merged_SMOTE) #AIC: 10502
vif(logistic_2_Merged_SMOTE)

# Removing No.of.times.60.DPD.or.worse.in.last.6.months due to high vif and less significance
logistic_3_Merged_SMOTE <- glm(Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + Education.xPhd +
                                 Presence.of.open.auto.loan + Income + No.of.months.in.current.residence + No.of.months.in.current.company +
                                 No.of.times.90.DPD.or.worse.in.last.6.months  + No.of.times.30.DPD.or.worse.in.last.6.months +
                                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months +
                                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + Total.No.of.Trades, family = "binomial", data = train_merged_SMOTE)
summary(logistic_3_Merged_SMOTE) #AIC: 10506
vif(logistic_3_Merged_SMOTE)

# Removing No.of.times.30.DPD.or.worse.in.last.6.months due to high vif and less significance
logistic_4_Merged_SMOTE <- glm(Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + Education.xPhd +
                                 Presence.of.open.auto.loan + Income + No.of.months.in.current.residence + No.of.months.in.current.company +
                                 No.of.times.90.DPD.or.worse.in.last.6.months +
                                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months +
                                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + Total.No.of.Trades, family = "binomial", data = train_merged_SMOTE)
summary(logistic_4_Merged_SMOTE) #AIC: 10507
vif(logistic_4_Merged_SMOTE)

# Removing Gender due to less significance
logistic_5_Merged_SMOTE <- glm(Performance.Tag ~  Marital.Status..at.the.time.of.application. + Education.xPhd +
                                 Presence.of.open.auto.loan + Income + No.of.months.in.current.residence + No.of.months.in.current.company +
                                 No.of.times.90.DPD.or.worse.in.last.6.months +
                                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months +
                                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + Total.No.of.Trades, family = "binomial", data = train_merged_SMOTE)
summary(logistic_5_Merged_SMOTE) #AIC: 10508
vif(logistic_5_Merged_SMOTE)

# Removing No.of.times.90.DPD.or.worse.in.last.6.months due to less significance and comparatively high vif
logistic_6_Merged_SMOTE <- glm(Performance.Tag ~  Marital.Status..at.the.time.of.application. + Education.xPhd +
                                 Presence.of.open.auto.loan + Income + No.of.months.in.current.residence + No.of.months.in.current.company +
                                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months +
                                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + Total.No.of.Trades, family = "binomial", data = train_merged_SMOTE)
summary(logistic_6_Merged_SMOTE) #AIC: 10507
vif(logistic_6_Merged_SMOTE)

# Removing No.of.months.in.current.company due to its less significance
logistic_7_Merged_SMOTE <- glm(Performance.Tag ~  Marital.Status..at.the.time.of.application. + Education.xPhd +
                                 Presence.of.open.auto.loan + Income + No.of.months.in.current.residence  +
                                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months +
                                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + Total.No.of.Trades, family = "binomial", data = train_merged_SMOTE)
summary(logistic_7_Merged_SMOTE) #AIC: 10508
vif(logistic_7_Merged_SMOTE)

# Removing No.of.PL.trades.opened.in.last.6.months due to high vif and less significance
logistic_8_Merged_SMOTE <- glm(Performance.Tag ~  Marital.Status..at.the.time.of.application. + Education.xPhd +
                                 Presence.of.open.auto.loan + Income + No.of.months.in.current.residence  +
                                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months +
                                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + Total.No.of.Trades, family = "binomial", data = train_merged_SMOTE)
summary(logistic_8_Merged_SMOTE) #AIC: 10511
vif(logistic_8_Merged_SMOTE)

# Removing Total.No.of.Trades due to high vif and less signifiance
logistic_9_Merged_SMOTE <- glm(Performance.Tag ~  Marital.Status..at.the.time.of.application. + Education.xPhd +
                                 Presence.of.open.auto.loan + Income + No.of.months.in.current.residence  +
                                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                 Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months +
                                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , family = "binomial", data = train_merged_SMOTE)
summary(logistic_9_Merged_SMOTE) #AIC: 10513
vif(logistic_9_Merged_SMOTE)

# Removing Presence.of.open.auto.loan due to less significance
logistic_10_Merged_SMOTE <- glm(Performance.Tag ~  Marital.Status..at.the.time.of.application. + Education.xPhd +
                                  Income + No.of.months.in.current.residence  +
                                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                  Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months +
                                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , family = "binomial", data = train_merged_SMOTE)
summary(logistic_10_Merged_SMOTE) #AIC: 10513

# Removing Education.xPhd due to less significance
logistic_11_Merged_SMOTE <- glm(Performance.Tag ~  Marital.Status..at.the.time.of.application. +
                                  Income + No.of.months.in.current.residence  +
                                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                  Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months +
                                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , family = "binomial", data = train_merged_SMOTE)
summary(logistic_11_Merged_SMOTE) #AIC: 10513

# Removing Income due to less significance
logistic_12_Merged_SMOTE <- glm(Performance.Tag ~  Marital.Status..at.the.time.of.application. +
                                  No.of.months.in.current.residence  +
                                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                  Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months +
                                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , family = "binomial", data = train_merged_SMOTE)
summary(logistic_12_Merged_SMOTE) #AIC: 10516

# Removing No.of.months.in.current.residence  due to less significance
logistic_13_Merged_SMOTE <- glm(Performance.Tag ~  Marital.Status..at.the.time.of.application. +
                                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                  Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months +
                                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , family = "binomial", data = train_merged_SMOTE)
summary(logistic_13_Merged_SMOTE) #AIC: 10518

# Removing Marital.Status..at.the.time.of.application. due to less significance
logistic_14_Merged_SMOTE <- glm(Performance.Tag ~
                                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months +
                                  Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months +
                                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. , family = "binomial", data = train_merged_SMOTE)
summary(logistic_14_Merged_SMOTE) #AIC: 10520

#With 6 significant variables in the model
temp_merged_SMOTE_log_model1 <- logistic_14_Merged_SMOTE


### Model Evaluation

test_merged_SMOTE <- test_No_WOE
test_merged_SMOTE <- test_merged_SMOTE[ , -which(names(test_merged_SMOTE) %in% c("prob"))]

#predicted probabilities of Performance for test data
test_pred = predict(temp_merged_SMOTE_log_model1, type = "response", 
                    newdata = test_merged_SMOTE[,-35])  

# Let's see the summary 
summary(test_pred)


# Add the prediction probability with the test data set for further model evaluation steps.
test_merged_SMOTE$prob <- test_pred

# View the test dataset including prediction probabity.
View(test_merged_SMOTE)

# Let's use the probability cutoff of 50%.
test_pred_performance <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_merged_SMOTE$Performance.Tag==1,"Yes","No"))

table(test_actual_performance,test_pred_performance)
#test_pred_performance
#test_actual_performance    No   Yes
#No  11706  7987
#Yes   270   597

test_conf <- confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

#Accuracy : 60%
#Sensitivity : 68%       
#Specificity : 60%

# Here we can see accuaracy, sensitivity and specificity of the model is too low.

# Creating cutoff values from 0.27 to 0.91 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

# create 100s of sequence number between 0.27 and 0.91, which will be used as cut-off value
# in each iteration to find out optimal value.
s = seq(.27,.91,length=100)

# Initialize a 100x3 matrix with 0 as default value.
OUT = matrix(0,100,3)

# Call the perform_fn in a loop and assign the output toOUT matrix for "sensitivity", "specificity", "accuracy".
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) +
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  lines(s,OUT[,2],col="darkgreen",lwd=2) +
  lines(s,OUT[,3],col=4,lwd=2) 

# Add legends to the plot created earlier.
legend(0.60,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy")) 

# Add a box over the plot.
box()

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.015)]

# Let's choose a cutoff value of 0.528 for final model

test_cutoff_performance <- factor(ifelse(test_pred >=0.522, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

# Get Accuracy, Sensitivity and Specificity from the confusion matrix using 

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

# Show Accuracy
acc
# Accuracy : 0.64
# Show Sensitivity
sens
# Sensitivity : 0.63
# Show Specificity
spec
# Specificity : 0.64

############ MODEL-5 ############## DECISION TREE ON MERGED DATASET WITHOUT SMOTE #############

#Taking backup for Dtree
combined_dataset_tree_no_smote <- combined_dataset


combined_dataset_tree_no_smote$Gender <- as.factor(combined_dataset_tree_no_smote$Gender)
combined_dataset_tree_no_smote$Marital.Status..at.the.time.of.application. <- as.factor(combined_dataset_tree_no_smote$Marital.Status..at.the.time.of.application.)
combined_dataset_tree_no_smote$Education <- as.factor(combined_dataset_tree_no_smote$Education)
combined_dataset_tree_no_smote$Profession <- as.factor(combined_dataset_tree_no_smote$Profession)
combined_dataset_tree_no_smote$Type.of.residence <- as.factor(combined_dataset_tree_no_smote$Type.of.residence)
combined_dataset_tree_no_smote$Presence.of.open.auto.loan <- as.factor(combined_dataset_tree_no_smote$Presence.of.open.auto.loan)
combined_dataset_tree_no_smote$Presence.of.open.home.loan <- as.factor(combined_dataset_tree_no_smote$Presence.of.open.home.loan)
combined_dataset_tree_no_smote$Performance.Tag <- as.factor(combined_dataset_tree_no_smote$Performance.Tag)

#Splitting into train and test data
set.seed(1)
tree_split_indices_no_smote <- sample.split(combined_dataset_tree_no_smote$Performance.Tag, SplitRatio = 0.50)

tree_train_No_smote <- combined_dataset_tree_no_smote[tree_split_indices_no_smote, ]
tree_test_No_smote <- combined_dataset_tree_no_smote[!tree_split_indices_no_smote, ]

table(tree_train_No_smote$Performance.Tag)

str(tree_train_No_smote)

#1 build tree model- default hyperparameters
tree_1_combined_no_smote <- rpart(Performance.Tag ~ .,         # formula
                                  data = tree_train_No_smote,                # training data
                                  method = "class")                        # classification or regression

# display decision tree
prp(tree_1_combined_no_smote)

# make predictions on the test set
tree_1_combined_no_smote_predict <- predict(tree_1_combined_no_smote, tree_test_No_smote, type = "class")

# evaluate the results
confusionMatrix(tree_1_combined_no_smote_predict, tree_test_No_smote$Performance.Tag, positive = '1')  # 0.95
#Since we are not getting expected Accuracy/Sensitivity and Specificity, we are trying to tune DT

#2 Change the algorithm to "information gain" instead of default "gini" ----------------------
tree_2_combined_no_smote <- rpart(Performance.Tag ~ .,                     # formula
                                  data = tree_train_No_smote,                   # training data
                                  method = "class",               # classification or regression
                                  parms = list(split = "information")
)

# display decision tree
prp(tree_2_combined_no_smote)

# make predictions on the test set
tree_2_combined_no_smote_predict <- predict(tree_2_combined_no_smote, tree_test_No_smote, type = "class")

# evaluate the results
confusionMatrix(tree_2_combined_no_smote_predict, tree_test_No_smote$Performance.Tag, positive = "1")  # 0.95

#3 Tune the hyperparameters ----------------------------------------------------------
tree_3_combined_no_smote <- rpart(Performance.Tag ~ .,                                # formula
                                  data = tree_train_No_smote,                             # training data
                                  method = "class",                         # classification or regression
                                  control = rpart.control(minsplit = 1000,  # min observations for node
                                                          minbucket = 1000, # min observations for leaf node
                                                          cp = 0.05))       # complexity parameter

# display decision tree
prp(tree_3_combined_no_smote)

# make predictions on the test set
tree_3_combined_no_smote_predict <- predict(tree_3_combined_no_smote, tree_test_No_smote, type = "class")

# evaluate the results
confusionMatrix(tree_3_combined_no_smote_predict, tree_test_No_smote$Performance.Tag, positive = "1")  # 0.95

#4 A more complex tree -----------------------------------------------------------------
tree_4_combined_no_smote <- rpart(Performance.Tag ~ .,                                # formula
                                  data = tree_train_No_smote,                             # training data
                                  method = "class",                         # classification or regression
                                  control = rpart.control(minsplit = 1,     # min observations for node
                                                          minbucket = 1,    # min observations for leaf node
                                                          cp = 0.001))      # complexity parameter

# display decision tree
prp(tree_4_combined_no_smote)

# make predictions on the test set
tree_4_combined_no_smote_predict <- predict(tree_4_combined_no_smote, tree_test_No_smote, type = "class")

# evaluate the results
confusionMatrix(tree_4_combined_no_smote_predict, tree_test_No_smote$Performance.Tag, positive = "1") # 0.95

#5 Cross test to choose CP ------------------------------------------------------------
library(caret)

# set the number of folds in cross test to 5
dtree.control.no.smote = trainControl(method = "cv", number = 5)

# set the search space for CP
dtree.grid.no.smote = expand.grid(cp = seq(0, 0.02, 0.0025))


# train model
tree_5_combined_no_smote <- train(Performance.Tag ~ .,
                                  data = tree_train_No_smote,
                                  method = "rpart",
                                  metric = "Accuracy",
                                  trControl = dtree.control.no.smote,
                                  tuneGrid = dtree.grid.no.smote,
                                  control = rpart.control(minsplit = 50,
                                                          minbucket = 20))

# look at cross validated model results
tree_5_combined_no_smote

# look at best value of hyperparameter
tree_5_combined_no_smote$bestTune  #0.0025

# make predictions on test set
tree_5_combined_no_smote_predict <- predict.train(tree_5_combined_no_smote, tree_test_No_smote)

# accuracy
confusionMatrix(tree_5_combined_no_smote_predict, tree_test_No_smote$Performance.Tag)   #0.95

# plot CP vs Accuracy
library(ggplot2)
accuracy_graph_no_smote <- data.frame(tree_5_combined_no_smote$results)
ggplot(data = accuracy_graph_no_smote, aes(x = cp, y = Accuracy*100)) +
  geom_line() +
  geom_point() +
  labs(x = "Complexity Parameter (CP)", y = "Accuracy", title = "CP vs Accuracy")


# As the data is highly unbalanced and more than 95% data are of one class the model is not able to trained properly,
# and data is data is kind of getting memorised by the model. Though accuracy of the model is high, this model is not acceptable for unforseen data.
# So we can't use Decision Tree on the merged dataset.
# Next we will try Decision Tree on the merged dataset, by balancing the dataset first

############ MODEL-6 ############## DECISION TREE ON MERGED DATASET WITH SMOTE #############

#Taking backup for Dtree
combined_dataset_tree <- combined_dataset

combined_dataset_tree$Gender <- as.factor(combined_dataset_tree$Gender)
combined_dataset_tree$Marital.Status..at.the.time.of.application. <- as.factor(combined_dataset_tree$Marital.Status..at.the.time.of.application.)
combined_dataset_tree$Education <- as.factor(combined_dataset_tree$Education)
combined_dataset_tree$Profession <- as.factor(combined_dataset_tree$Profession)
combined_dataset_tree$Type.of.residence <- as.factor(combined_dataset_tree$Type.of.residence)
combined_dataset_tree$Presence.of.open.auto.loan <- as.factor(combined_dataset_tree$Presence.of.open.auto.loan)
combined_dataset_tree$Presence.of.open.home.loan <- as.factor(combined_dataset_tree$Presence.of.open.home.loan)
combined_dataset_tree$Performance.Tag <- as.factor(combined_dataset_tree$Performance.Tag)


#Splitting into train and test data
set.seed(1)
tree_split_indices <- sample.split(combined_dataset_tree$Performance.Tag, SplitRatio = 0.70)

tree_train_No_WOE <- combined_dataset_tree[tree_split_indices, ]
tree_test_No_WOE <- combined_dataset_tree[!tree_split_indices, ]

tree_train_Smote_No_WOE <- SMOTE(Performance.Tag ~ ., tree_train_No_WOE, perc.over = 100,perc.under=200)
table(tree_train_Smote_No_WOE$Performance.Tag)

#1 build tree model- default hyperparameters
tree_1_combined_no_woe <- rpart(Performance.Tag ~ .,         # formula
                                data = tree_train_Smote_No_WOE,                # training data
                                method = "class")                        # classification or regression

# display decision tree
prp(tree_1_combined_no_woe)

# make predictions on the test set
tree_1_combined_no_woe_predict <- predict(tree_1_combined_no_woe, tree_test_No_WOE, type = "class")

# evaluate the results
confusionMatrix(tree_1_combined_no_woe_predict, tree_test_No_WOE$Performance.Tag, positive = '1')  # 0.6764
#With the base tree, we are getting Accuracy of 65%.


# Tuning the tree parameters to get better accuracy
#2 Change the algorithm to "information gain" instead of default "gini" ----------------------
tree_2_combined_no_woe <- rpart(Performance.Tag ~ .,                     # formula
                                data = tree_train_Smote_No_WOE,                   # training data
                                method = "class",               # classification or regression
                                parms = list(split = "information")
)

# display decision tree
prp(tree_2_combined_no_woe)

# make predictions on the test set
tree_2_combined_no_woe_predict <- predict(tree_2_combined_no_woe, tree_test_No_WOE, type = "class")

# evaluate the results
confusionMatrix(tree_2_combined_no_woe_predict, tree_test_No_WOE$Performance.Tag, positive = "1")  # 0.6706


#3 Tune the hyperparameters ----------------------------------------------------------
tree_3_combined_no_woe <- rpart(Performance.Tag ~ .,                                # formula
                                data = tree_train_Smote_No_WOE,                             # training data
                                method = "class",                         # classification or regression
                                control = rpart.control(minsplit = 1000,  # min observations for node
                                                        minbucket = 1000, # min observations for leaf node
                                                        cp = 0.05))       # complexity parameter

# display decision tree
prp(tree_3_combined_no_woe)

# make predictions on the test set
tree_3_combined_no_woe_predict <- predict(tree_3_combined_no_woe, tree_test_No_WOE, type = "class")

# evaluate the results
confusionMatrix(tree_3_combined_no_woe_predict, tree_test_No_WOE$Performance.Tag, positive = "1")  # 0.6424

#4 A more complex tree -----------------------------------------------------------------
tree_4_combined_no_woe <- rpart(Performance.Tag ~ .,                                # formula
                                data = tree_train_Smote_No_WOE,                             # training data
                                method = "class",                         # classification or regression
                                control = rpart.control(minsplit = 1,     # min observations for node
                                                        minbucket = 1,    # min observations for leaf node
                                                        cp = 0.001))      # complexity parameter

# display decision tree
prp(tree_4_combined_no_woe)

# make predictions on the test set
tree_4_combined_no_woe_predict <- predict(tree_4_combined_no_woe, tree_test_No_WOE, type = "class")

# evaluate the results
confusionMatrix(tree_4_combined_no_woe_predict, tree_test_No_WOE$Performance.Tag, positive = "1") # 0.7084

#5 Cross test to choose CP ------------------------------------------------------------
library(caret)

# set the number of folds in cross test to 5
dtree.control = trainControl(method = "cv", number = 5)

# set the search space for CP
dtree.grid = expand.grid(cp = seq(0, 0.02, 0.0025))


# train model
tree_5_combined_no_woe <- train(Performance.Tag ~ .,
                                data = tree_train_Smote_No_WOE,
                                method = "rpart",
                                metric = "Accuracy",
                                trControl = dtree.control,
                                tuneGrid = dtree.grid,
                                control = rpart.control(minsplit = 50,
                                                        minbucket = 20))

# look at cross validated model results
tree_5_combined_no_woe

# look at best value of hyperparameter
tree_5_combined_no_woe$bestTune  #0.0025

# make predictions on test set
tree_5_combined_no_woe_predict <- predict.train(tree_5_combined_no_woe, tree_test_No_WOE)

# accuracy
confusionMatrix(tree_5_combined_no_woe_predict, tree_test_No_WOE$Performance.Tag)   #0.7987

# plot CP vs Accuracy
library(ggplot2)
accuracy_graph <- data.frame(tree_5_combined_no_woe$results)
ggplot(data = accuracy_graph, aes(x = cp, y = Accuracy*100)) +
  geom_line() +
  geom_point() +
  labs(x = "Complexity Parameter (CP)", y = "Accuracy", title = "CP vs Accuracy")

############ MODEL-7 ############## RANDOM FOREST ON COMBINED DATASET WITHOUT SMOTE #############

#Random Forest Model Building on the combined dataset using the unbalanced data(without SMOTE)

str(combined_dataset)

# Taking a backup
combined_dataset_NO_Smote_RF <- combined_dataset
str(combined_dataset_NO_Smote_RF)

#Removing the derived columns (age_grp and salary_grp) as they will result in multicollinearity
#Removing the Application.ID column
combined_dataset_NO_Smote_RF <- combined_dataset_NO_Smote_RF[ , -which(names(combined_dataset_NO_Smote_RF) %in% c("age_grp","salary_grp","Application.ID"))]

#Creating vector for numeric variables
numeric_cols <- c('Age', 'No.of.dependents', 'Income', 'No.of.months.in.current.residence', 'No.of.months.in.current.company',
                  'No.of.times.90.DPD.or.worse.in.last.6.months', 'No.of.times.60.DPD.or.worse.in.last.6.months',
                  'No.of.times.30.DPD.or.worse.in.last.6.months', 'No.of.times.90.DPD.or.worse.in.last.12.months',
                  'No.of.times.60.DPD.or.worse.in.last.12.months', 'No.of.times.30.DPD.or.worse.in.last.12.months',
                  'Avgas.CC.Utilization.in.last.12.months','No.of.trades.opened.in.last.6.months',
                  'No.of.trades.opened.in.last.12.months', 'No.of.PL.trades.opened.in.last.6.months',
                  'No.of.PL.trades.opened.in.last.12.months', 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.',
                  'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.', 'Outstanding.Balance',
                  'Total.No.of.Trades')

#Creating vector for categorical variables
factor_cols <- c('Gender', 'Marital.Status..at.the.time.of.application.', 'Education', 'Profession',
                 'Type.of.residence','Presence.of.open.auto.loan','Presence.of.open.home.loan','Performance.Tag')

combined_dataset_NO_Smote_RF[, numeric_cols] <- lapply(numeric_cols, function(x) as.numeric(as.character(combined_dataset_NO_Smote_RF[, x])))
combined_dataset_NO_Smote_RF[, factor_cols] <- lapply(factor_cols, function(x) as.factor(as.character(combined_dataset_NO_Smote_RF[, x])))

# Shuffling the dataset to ensure that the data is not biased
set.seed(1)
combined_dataset_NO_Smote_RF <- combined_dataset_NO_Smote_RF[sample(nrow(combined_dataset_NO_Smote_RF)), ]

# Splitting the data into train and test
ntrain <- as.integer(nrow(combined_dataset_NO_Smote_RF)*0.7)
train_NO_Smote_RF <- combined_dataset_NO_Smote_RF[1:ntrain, ]
test_NO_Smote_RF <- combined_dataset_NO_Smote_RF[(ntrain+1):nrow(combined_dataset_NO_Smote_RF), ]

# Model Building the random forest

random_forest_No_smote <- randomForest(Performance.Tag ~ ., data=train_NO_Smote_RF, proximity=FALSE,
                                       ntree=500, mtry=5, do.trace=TRUE, na.action=na.omit)

random_forest_No_smote

#Finding optimal value of mtry

mtry_No_smote <- tuneRF(train_NO_Smote_RF[,-28],train_NO_Smote_RF$Performance.Tag, ntreeTry=200,
                        stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best_m_No_smote <- mtry_No_smote[mtry_No_smote[, 2] == min(mtry_No_smote[, 2]), 1]
print(mtry_No_smote)
print(best_m_No_smote)

random_forest_No_smote <- randomForest(Performance.Tag ~ ., data=train_NO_Smote_RF, proximity=FALSE,
                                       ntree=200, mtry=best_m_No_smote, do.trace=TRUE, na.action=na.omit)


#Predicting on the test data
test_pred_no_smote_rf <- predict(random_forest_No_smote, newdata=test_NO_Smote_RF)
table(test_pred_no_smote_rf, test_NO_Smote_RF$Performance.Tag)

#test_pred_no_smote_rf     0     1
#                     0 19723   838
#                     1     0     0

#Confusion Matrix
test_conf_no_smote_rf <- confusionMatrix(test_pred_no_smote_rf, test_NO_Smote_RF$Performance.Tag)
test_conf_no_smote_rf

#Accuracy : 95.75%
#Sensitivity: 100%
#Specificity: 0%

#Random Forest is not working on the unbalanced dataset. Hence we will balance the dataset and try.


############ MODEL-8 ############## RANDOM FOREST ON COMBINED DATASET WITH SMOTE #############

#Random Forest Model Building on the combined dataset using the balanced data(with SMOTE)

#Taking a backup of the unbalanced train data train_NO_Smote_RF to see the data spread
combined_dataset_No_Smote_RF_train <- train_NO_Smote_RF

table(combined_dataset_No_Smote_RF_train$Performance.Tag)
#0     1 
#45922  2053 
#Clearly the dataset is not balanced because number of 0 is more than number of 1.

combined_dataset_Smote_RF_train <- SMOTE(Performance.Tag ~ ., combined_dataset_No_Smote_RF_train, perc.over = 100,k=5,perc.under=200)
table(combined_dataset_Smote_RF_train$Performance.Tag)


# Model Building the random forest
set.seed(1)
random_forest_smote <- randomForest(Performance.Tag ~ ., data=combined_dataset_Smote_RF_train, proximity=FALSE,
                                    ntree=500, mtry=5, do.trace=TRUE, na.action=na.omit)

random_forest_smote

#Finding optimal value of mtry

mtry_smote <- tuneRF(combined_dataset_Smote_RF_train[,-28],combined_dataset_Smote_RF_train$Performance.Tag, ntreeTry=500,
                     stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best_m_smote <- mtry_smote[mtry_smote[, 2] == min(mtry_smote[, 2]), 1]
print(mtry_smote)
print(best_m_smote)

random_forest_smote <- randomForest(Performance.Tag ~ ., data=combined_dataset_Smote_RF_train, proximity=FALSE,
                                    ntree=200, mtry=best_m_smote, do.trace=TRUE, na.action=na.omit)


#Predicting on the test data
test_Smote_RF <-test_NO_Smote_RF
test_pred_smote_rf <- predict(random_forest_smote, newdata=test_Smote_RF)
table(test_pred_smote_rf, test_Smote_RF$Performance.Tag)

#test_pred_smote_rf     0     1
#                     0 15419   524
#                     1 4297    321

#Confusion Matrix
test_conf_smote_rf <- confusionMatrix(test_pred_smote_rf, test_Smote_RF$Performance.Tag)
test_conf_smote_rf

#Accuracy : 76.55%
#Sensitivity: 78.20%
#Specificity: 37.98%

############ MODEL-9 ############## RANDOM FOREST ON DEMOGRAPHIC DATASET WITHOUT SMOTE #############

#Random Forest Model Building on the demographic dataset using the unbalanced data(without SMOTE)

str(combined_dataset)

# Taking a backup
demographic_NO_Smote_RF <- combined_dataset %>%
  dplyr::select(names(demographic[,-12]),Performance.Tag)

str(demographic_NO_Smote_RF)

#Removing the Application.ID column
demographic_NO_Smote_RF <- demographic_NO_Smote_RF[ , -which(names(demographic_NO_Smote_RF) %in% c("Application.ID"))]

#Creating vector for numeric variables
numeric_cols <- c('Age', 'No.of.dependents', 'Income', 'No.of.months.in.current.residence', 'No.of.months.in.current.company')

#Creating vector for categorical variables
factor_cols <- c('Gender', 'Marital.Status..at.the.time.of.application.', 'Education', 'Profession',
                 'Type.of.residence', 'Performance.Tag')

demographic_NO_Smote_RF[, numeric_cols] <- lapply(numeric_cols, function(x) as.numeric(as.character(demographic_NO_Smote_RF[, x])))
demographic_NO_Smote_RF[, factor_cols] <- lapply(factor_cols, function(x) as.factor(as.character(demographic_NO_Smote_RF[, x])))

# Shuffling the dataset to ensure that the data is not biased
set.seed(100)
demographic_NO_Smote_RF <- demographic_NO_Smote_RF[sample(nrow(demographic_NO_Smote_RF)), ]

# Splitting the data into train and test
ntrain <- as.integer(nrow(demographic_NO_Smote_RF)*0.7)
demographic_train_NO_Smote_RF <- demographic_NO_Smote_RF[1:ntrain, ]
demographic_test_NO_Smote_RF <- demographic_NO_Smote_RF[(ntrain+1):nrow(demographic_NO_Smote_RF), ]

# Model Building the random forest
random_forest_demographic_No_smote <- randomForest(Performance.Tag ~ ., data=demographic_train_NO_Smote_RF, proximity=FALSE,
                                                   ntree=500, mtry=5, do.trace=TRUE, na.action=na.omit)

random_forest_demographic_No_smote

#Predicting on the test data
demographic_test_pred_no_smote_rf <- predict(random_forest_demographic_No_smote, newdata=demographic_test_NO_Smote_RF)
table(demographic_test_pred_no_smote_rf, demographic_test_NO_Smote_RF$Performance.Tag)

#test_pred_no_smote_rf     0     1
#                     0 19663   898
#                     1     0     0

#Confusion Matrix
demographic_test_conf_no_smote_rf <- confusionMatrix(demographic_test_pred_no_smote_rf, demographic_test_NO_Smote_RF$Performance.Tag)
demographic_test_conf_no_smote_rf

#Accuracy : 95.63%
#Sensitivity: 100%
#Specificity: 0%


############ MODEL-10 ############## RANDOM FOREST ON DEMOGRAPHIC DATASET WITH SMOTE #############

#Random Forest Model Building on the demographic dataset using the balanced data(with SMOTE)

#Checking the spread of performance.tag in the demographic_train_NO_Smote_RF dataset

table(demographic_train_NO_Smote_RF$Performance.Tag)
#0     1 
#45982  1993 
#Clearly the dataset is not balanced because number of 0 is more than number of 1.

demographic_Smote_RF_train <- SMOTE(Performance.Tag ~ ., demographic_train_NO_Smote_RF, perc.over = 100,k=5,perc.under=200)
table(demographic_Smote_RF_train$Performance.Tag)

# Model Building the random forest
set.seed(100)
random_forest_demographic_smote <- randomForest(Performance.Tag ~ ., data=demographic_Smote_RF_train, proximity=FALSE,
                                                ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)

random_forest_demographic_smote

#Finding optimal value of mtry

mtry_smote <- tuneRF(demographic_Smote_RF_train[,-11],demographic_Smote_RF_train$Performance.Tag, ntreeTry=500,
                     stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best_m_smote <- mtry_smote[mtry_smote[, 2] == min(mtry_smote[, 2]), 1]
print(mtry_smote)
print(best_m_smote)

random_forest_demographic_smote <- randomForest(Performance.Tag ~ ., data=demographic_Smote_RF_train, proximity=FALSE,
                                                ntree=200, mtry=best_m_smote, do.trace=TRUE, na.action=na.omit)


#Predicting on the test data
demographic_test_Smote_RF <-demographic_test_NO_Smote_RF
demographic_test_pred_smote_rf <- predict(random_forest_demographic_smote, newdata=demographic_test_Smote_RF)
table(demographic_test_pred_smote_rf, demographic_test_Smote_RF$Performance.Tag)

#demographic_test_pred_smote_rf     0     1
#                             0 14794   582
#                             1  4869   316

#Confusion Matrix
demographic_test_conf_smote_rf <- confusionMatrix(demographic_test_pred_smote_rf, demographic_test_Smote_RF$Performance.Tag)
demographic_test_conf_smote_rf

#Accuracy : 73.49%
#Sensitivity: 75.23%
#Specificity: 35.18%

##########################################
#Final Model on Demographic Dataset - demographic_test_conf_smote_rf
#Final Model on Merged Dataset - random_forest_smote

######################################################
#############Application_Scorecard###########
##########################################

#Calculating the probabilities based on the final model on merged dataset - random_forest_smote

test_Smote_RF$predict_default <- predict(random_forest_smote,newdata = test_Smote_RF[,-28],type='prob')
test_Smote_RF$predict_non_default <- (1-(test_Smote_RF$predict_default))

test_Smote_RF$odds <-  log(test_Smote_RF$predict_non_default/test_Smote_RF$predict_default)

PDO <- 20
BaseScore <- 400
Odds <- 10

#Calculating Factor & Offset
Factor=PDO/log(2)

Offset=BaseScore-(Factor*log(Odds))

Offset
#333.5614

Factor
#28.8539

print("equation is : score = 333.5614 + (28.8539 * log(odds))")

test_Smote_RF$score <- 333.5614 + (28.8539 * test_Smote_RF$odds)

summary(test_Smote_RF$score)

quantile(test_Smote_RF$score,seq(0,1,0.2))

#From the above, it is evident that the cut off could be set to 359
cutoff_scorecard = 359

#Calculating the percentage of defaults below the cutoff value of 359
no_of_defaults_under_cutoff <- length(which(test_Smote_RF$Performance.Tag==1 & test_Smote_RF$score[,2]<cutoff_scorecard))
total_no_of_defaults<-length(which(test_Smote_RF$Performance.Tag==1))

percentage_of_defaults_under_cutoff <- ceiling((no_of_defaults_under_cutoff/total_no_of_defaults)*100)

percentage_of_defaults_under_cutoff

#Plotting the score distribution for all the applicants of the test data
ggplot(test_Smote_RF, aes(x = score[,2],color=Performance.Tag))+geom_bar()+geom_vline(aes(xintercept = cutoff_scorecard))+
  labs(x="Score",y="Count",title="Score Distribution for all applicants")+
  annotate("text", x=350,y=4000, colour = "black",hjust=0, vjust=0, size=4,label=paste("Defaults covered by cut off of 359: " ,percentage_of_defaults_under_cutoff,"%"))

#########################################################################
#Predicting the application scores for the rejected population

rejected_applicants_df <- combined_df_NA

#Removing the Applicant Id field
rejected_applicants_df <- rejected_applicants_df[ , -which(names(rejected_applicants_df) %in% c("Application.ID"))]
str(rejected_applicants_df)

#Creating vector for numeric variables
numeric_cols_rej <- c('Age', 'No.of.dependents', 'Income', 'No.of.months.in.current.residence', 'No.of.months.in.current.company',
                      'No.of.times.90.DPD.or.worse.in.last.6.months', 'No.of.times.60.DPD.or.worse.in.last.6.months',
                      'No.of.times.30.DPD.or.worse.in.last.6.months', 'No.of.times.90.DPD.or.worse.in.last.12.months',
                      'No.of.times.60.DPD.or.worse.in.last.12.months', 'No.of.times.30.DPD.or.worse.in.last.12.months',
                      'Avgas.CC.Utilization.in.last.12.months','No.of.trades.opened.in.last.6.months',
                      'No.of.trades.opened.in.last.12.months', 'No.of.PL.trades.opened.in.last.6.months',
                      'No.of.PL.trades.opened.in.last.12.months', 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.',
                      'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.', 'Outstanding.Balance',
                      'Total.No.of.Trades')

#Creating vector for categorical variables
factor_cols_rej <- c('Gender', 'Marital.Status..at.the.time.of.application.', 'Education', 'Profession',
                     'Type.of.residence','Presence.of.open.auto.loan','Presence.of.open.home.loan')

#Converting the types for the numeric_cols_rej and factor_cols_rej
rejected_applicants_df[, numeric_cols_rej] <- lapply(numeric_cols_rej, function(x) as.numeric(as.character(rejected_applicants_df[, x])))
rejected_applicants_df[, factor_cols_rej] <- lapply(factor_cols_rej, function(x) as.factor(as.character(rejected_applicants_df[, x])))

#Calculating the probabilities for the rejected population based on the final model (random forest on merged dataset with smote)

rejected_applicants_df$predict_default <- predict(random_forest_smote,newdata = rejected_applicants_df,type='prob')
rejected_applicants_df$predict_non_default <- (1-(rejected_applicants_df$predict_default))

rejected_applicants_df$odds <-  log(rejected_applicants_df$predict_non_default/rejected_applicants_df$predict_default)

rejected_applicants_df$score <- ceiling(Offset + (Factor*rejected_applicants_df$odds))

summary(rejected_applicants_df$score)

cutoff_scorecard = 359

length(which(rejected_applicants_df$score[,2]<cutoff_scorecard))/(nrow(rejected_applicants_df)) 
# Using the decided cutoff of 359 we were able to identify 97.26% actual rejected applicants.

actual_rejections_using_scorecard = "97.26%"

#Plotting the score distribution for actual rejected applicants
ggplot(rejected_applicants_df, aes(x = score[,2])) +geom_bar()+ geom_vline(aes(xintercept = cutoff_scorecard,col="red"))+
  labs(x="Score",y="Count",title="Score Distribution of Actual Rejected applications")+
  annotate("text", x=380,y=1, colour = "white",hjust=0, vjust=0, size=7,label=paste("Corect rejections by score card% =", actual_rejections_using_scorecard)) 

######################################################
#############Financial analysis##############

#From the above we can infer that credit card can be provided to approximately 3% 
#out of all the rejected applicants.
#Using the above suggested cutoff of 359 the credit loss would be minimised.
