########################HR Analytics Solution###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################
# Install and Load the required packages
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("caTools")
#install.packages("ROCR")

#Loading the packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(GGally)
library(caTools)
library(ROCR)
#############Data Loading and Data Preparation stage################

#Loading the files
employee_survey <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE, na.strings = c('NA','na','Na'))
general_data <- read.csv("general_data.csv", stringsAsFactors = FALSE,na.strings = c('NA','na','Na'))
manager_survey <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE, na.strings = c('NA','na','Na'))
in_time <- read.csv("in_time.csv", stringsAsFactors=FALSE, na.strings = c('NA','na','Na'),check.names = F)
out_time <- read.csv("out_time.csv", stringsAsFactors=FALSE, na.strings = c('NA','na','Na'),check.names = F)

#Converting in_time wide to long format
in_time <- gather(in_time, date,in_timestamp, -1)
colnames(in_time)[1] <- 'EmployeeID'

#Converting out_time wide to long format
out_time <- gather(out_time, date,out_timestamp, -1)
colnames(out_time)[1] <- 'EmployeeID'

#Merging in_time and out_time by EmployeeID and date. Removing NA data
In_Time_Out_Time <- merge(in_time,out_time,by=c('EmployeeID','date'))

sum(is.na(In_Time_Out_Time$in_timestamp)) #109080
sum(is.na(In_Time_Out_Time$out_timestamp)) #109080

In_Time_Out_Time <- na.omit(In_Time_Out_Time) #Removing NA data

#Converting timestamp to uniform format
DateConvert <- function(date){
  date <- as.POSIXct(date,format = "%Y-%m-%d %H:%M:%S",origin="1970-01-01")
  }
  
In_Time_Out_Time <- transform(In_Time_Out_Time,date =as.POSIXct(date,format = "%Y-%m-%d"),
                        in_timestamp =DateConvert(in_timestamp),
                        out_timestamp =DateConvert(out_timestamp))

#Calculating daily working hours for each employee and the Quarter
In_Time_Out_Time <- mutate(In_Time_Out_Time,working_hours = round(difftime(out_timestamp,in_timestamp),2),
                     qtr = quarters(date))

#Calculating quaterly average working hours and annual average working hours for each employee

In_Time_Out_Time <- 
  In_Time_Out_Time %>%
  group_by(EmployeeID,qtr) %>%
  mutate(avg_qtr_hours = sum(working_hours)/n())

In_Time_Out_Time <- 
  In_Time_Out_Time %>%
  group_by(EmployeeID) %>%
  mutate(avg_annual_hours = sum(working_hours)/n())

#Extracting distinct employees and their corresponding quaterly and annual average working hours

working_hours <- distinct(In_Time_Out_Time[,c(1,6,7,8)])
working_hours$avg_qtr_hours <- round(working_hours$avg_qtr_hours,2)
working_hours$avg_annual_hours <- round(working_hours$avg_annual_hours,2)

#Collate the data to a single file
length(unique(employee_survey$EmployeeID)) #4410
length(unique(manager_survey$EmployeeID)) #4410
length(unique(general_data$EmployeeID)) #4410
length(unique(working_hours$EmployeeID)) #4410

setdiff(employee_survey$EmployeeID, manager_survey$EmployeeID) #Identical EmployeeID across these datasets
setdiff(manager_survey$EmployeeID, general_data$EmployeeID) #Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID, working_hours$EmployeeID) #Identical EmployeeID across these datasets

employee_data <- merge(employee_survey, manager_survey, by='EmployeeID', all = F)
employee_data <- merge(employee_data, general_data, by='EmployeeID', all=F)
employee_data <- merge(employee_data,working_hours, by='EmployeeID', all = F)

View(employee_data)

################################################################

### Data Preparation & Exploratory Data Analysis

#We have calculated the avg_qtr_hours and avg_annual_hours for each employee
#Since both the fields are highly correlated (~0.999) we are removing the avg_qtr_hours
employee_data$avg_annual_hours <- as.numeric(employee_data$avg_annual_hours)
employee_data$avg_qtr_hours <- as.numeric(employee_data$avg_qtr_hours)
cor(employee_data$avg_annual_hours,employee_data$avg_qtr_hours)

employee_data <- employee_data[,-c(30,31)] #Removing the qtr and avg_qtr_hours column

#Taking distinct employee_data
employee_data <- distinct(employee_data)

#Understanding the structure of the employee_data
str(employee_data)

# Age, DistanceFromHome, MonthlyIncome, PercentSalaryHike, TotalWorkingYears, YearsAtCompany,
#YearsSinceLastPromotion, YearsWithCurrManager and avg_annual_hours are continuous

# EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,JobInvolvement,PerformanceRating,
# Education, JobLevel, NumCompaniesWorked, StockOptionLevel and TrainingTimesLastYear 
# needs to be changed from integer to categorical

# Plotted Barcharts for categorical features with stacked employee attrition information
bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

#dev.off()

plot_grid(ggplot(employee_data, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar() + xlab("EnvironmentSatisfaction"), 
          ggplot(employee_data, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme + xlab("JobSatisfaction"),
          ggplot(employee_data, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme + xlab("WorkLifeBalance"),
          ggplot(employee_data, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme + xlab("JobInvolvement"),
          ggplot(employee_data, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme + xlab("PerformanceRating"),
          ggplot(employee_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme,
          align = "h")   

plot_grid(ggplot(employee_data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(employee_data, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme + xlab("Education"),
          ggplot(employee_data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(employee_data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(employee_data, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar()+bar_theme + xlab("JobLevel"),
          ggplot(employee_data, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme,
          align = "h") 

plot_grid(ggplot(employee_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(employee_data, aes(x=factor(NumCompaniesWorked),fill=Attrition))+ geom_bar()+bar_theme + xlab("NumCompaniesWorked"),
          ggplot(employee_data, aes(x=factor(StockOptionLevel),fill=Attrition))+ geom_bar()+bar_theme + xlab("StockOptionLevel"),
          ggplot(employee_data, aes(x=factor(TrainingTimesLastYear),fill=Attrition))+ geom_bar()+bar_theme +xlab("TrainingTimesLastYear"),
          align = "h") 

#From the above EDA, Attrition might be dependent on JobSatisfaction,WorkLifeBalance,
#JobInvolvement, PerformanceRating, BusinessTravel

#Male employees are more prone to attrition compared to female
#Attrition rate in Research & Development is more compared to other departments
#Attrition rate is more initial job levels
#Attrition rate is more when the employee is single
#Attrition rate is more where the employees have worked at one company (NumCompaniesWorked)

# Plotted Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(employee_data, aes(Age))+ geom_histogram(binwidth = 5) + ggtitle("Histogram for Age"),
          ggplot(employee_data, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for Age"), 
          align = "v",ncol = 1)

plot_grid(ggplot(employee_data, aes(DistanceFromHome))+ geom_histogram(binwidth = 5) + ggtitle("Histogram for DistanceFromHome"),
          ggplot(employee_data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for DistanceFromHome"), 
          align = "v",ncol = 1)

plot_grid(ggplot(employee_data, aes(MonthlyIncome))+ geom_histogram(binwidth = 20000) + ggtitle("Histogram for MonthlyIncome"),
          ggplot(employee_data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for MonthlyIncome"), 
          align = "v",ncol = 1)

plot_grid(ggplot(employee_data, aes(PercentSalaryHike))+ geom_histogram(binwidth = 2) + ggtitle("Histogram for PercentSalaryHike"),
          ggplot(employee_data, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for PercentSalaryHike"), 
          align = "v",ncol = 1)

plot_grid(ggplot(employee_data, aes(TotalWorkingYears))+ geom_histogram(binwidth = 5) + ggtitle("Histogram for TotalWorkingYears"),
          ggplot(employee_data, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for TotalWorkingYears"), 
          align = "v",ncol = 1)

plot_grid(ggplot(employee_data, aes(YearsAtCompany))+ geom_histogram(binwidth = 5) + ggtitle("Histogram for YearsAtCompany"),
          ggplot(employee_data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for YearsAtCompany"), 
          align = "v",ncol = 1)

plot_grid(ggplot(employee_data, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 3) + ggtitle("Histogram for YearsSinceLastPromotion"),
          ggplot(employee_data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for YearsSinceLastPromotion"), 
          align = "v",ncol = 1)

plot_grid(ggplot(employee_data, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 3) + ggtitle("Histogram for YearsWithCurrManager"),
          ggplot(employee_data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for YearsWithCurrManager"), 
          align = "v",ncol = 1)

plot_grid(ggplot(employee_data, aes(avg_annual_hours))+ geom_histogram(binwidth = 6) + ggtitle("Histogram for avg_annual_hours"),
          ggplot(employee_data, aes(x="",y=avg_annual_hours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for Age"), 
          align = "v",ncol = 1)

# Clear outliers in numeric variables like MonthlyIncome, TotalWorkingYears, YearsAtCompany,
#YearsSinceLastPromotion, YearsWithCurrManager and avg_annual_hours

# Boxplots of numeric variables relative to Attrition status
plot_grid(ggplot(employee_data, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Less Age sees more Attrition

plot_grid(ggplot(employee_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_data, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# The less the TotalWorkingYears and YearsAtCompany the more is the Attrition

plot_grid(ggplot(employee_data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_data, aes(x=Attrition,y=avg_annual_hours, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# The less the YearsWithCurrManager more is the Attrition
# The more the avg_annual_hours of an employee more is the Attrition

# Plotted the Correlation between numeric variables

ggpairs(employee_data[, c("Age", "DistanceFromHome", "MonthlyIncome", "PercentSalaryHike",
                          "TotalWorkingYears","YearsAtCompany","YearsSinceLastPromotion",
                          "YearsWithCurrManager","avg_annual_hours")])

# Age and MonthlyIncome are negatively correlated (~-0.04)
# YearsAtCompany and YearsWithCurrManager are highly correlated (~0.76)
# YearsAtCompany and TotalWorkingYears are moderately correlated (~0.63)
# YearsAtCompany and YearsSinceLastPromotion are moderately correlated (~0.61)

################################################################
### Data Preparation

#checking for any duplicate rows in the dataset
sum(duplicated(employee_data))

#Since the sum is returning zero signifying that the total no. of observations in the dataset is unique,
#indicating that there were no duplicates in the data set.

# Missing value
sum(is.na(employee_data)) # It shows all 111 NAs in employee_data

# It means that 111/4410 = 0.02517 i.e 2.52% of the data is NA,
# Since it is nominal hence removing these observations from the analysis
employee_data <- na.omit(employee_data)

# Bringing the variables in the correct format
#EnvironmentSatisfaction
employee_data$EnvironmentSatisfaction <- as.factor(employee_data$EnvironmentSatisfaction)
levels(employee_data$EnvironmentSatisfaction)<-c("Low","Medium","High","Very High")

#JobSatisfaction
employee_data$JobSatisfaction <- as.factor(employee_data$JobSatisfaction)
levels(employee_data$JobSatisfaction)<-c("Low","Medium","High","Very High")

#WorkLifeBalance
employee_data$WorkLifeBalance <- as.factor(employee_data$WorkLifeBalance)
levels(employee_data$WorkLifeBalance)<-c("Bad","Good","Better","Best")

#JobInvolvement
employee_data$JobInvolvement <- as.factor(employee_data$JobInvolvement)
levels(employee_data$JobInvolvement)<-c("Low","Medium","High","Very High")

#PerformanceRating
employee_data$PerformanceRating <- as.factor(employee_data$PerformanceRating)
levels(employee_data$PerformanceRating)<-c("Excellent","Outstanding")

#Education
employee_data$Education <- as.factor(employee_data$Education)
levels(employee_data$Education)<-c("Below College","College","Bachelor","Master","Doctor")

# Outlier treatment and imputing missing value
#Boxplot showed outliers for MonthlyIncome, TotalWorkingYears, YearsAtCompany,
#YearsSinceLastPromotion, YearsWithCurrManager and avg_annual_hours. 
#Here confirming it with percentiles for all the continuous variables
sapply(employee_data[,c("Age", "DistanceFromHome", "MonthlyIncome", "PercentSalaryHike",
                        "TotalWorkingYears","YearsAtCompany","YearsSinceLastPromotion",
                        "YearsWithCurrManager","avg_annual_hours")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T))

#There is a sudden jump in MonthlyIncome values between 90% and 91%.
#So, all values above 137756.0 (90%) are capped to 137756.0.
employee_data$MonthlyIncome[which(employee_data$MonthlyIncome>137756.0)] <- 137756.0

#There is a sudden jump in TotalWorkingYears values between 98% and 99%.
#So, all values above 32 (98%) are capped to 32.
employee_data$TotalWorkingYears[which(employee_data$TotalWorkingYears>32)] <- 32

#There is a sudden jump in YearsAtCompany values between 99% and 100%.
#So, all values above 31 (99%) are capped to 31.
employee_data$YearsAtCompany[which(employee_data$YearsAtCompany>31)] <- 31

#There is a sudden jump in YearsSinceLastPromotion values between 95% and 96%.
#So, all values above 9 (95%) are capped to 9.
employee_data$YearsSinceLastPromotion[which(employee_data$YearsSinceLastPromotion>9)] <- 9

#There is a sudden jump in YearsWithCurrManager values between 99% and 100%.
#So, all values above 14 (99%) are capped to 14.
employee_data$YearsWithCurrManager[which(employee_data$YearsWithCurrManager>14)] <- 14

#There is a sudden jump in avg_annual_hours values between 91% and 92%.
#So, all values above 10.1100 (91%) are capped to 10.1100.
employee_data$avg_annual_hours[which(employee_data$avg_annual_hours>10.1100)] <- 10.1100

################################################################
# Variable standardisation

# Normalising continuous variables 
employee_data$Age<- scale(employee_data$Age)
employee_data$DistanceFromHome<- scale(employee_data$DistanceFromHome)
employee_data$MonthlyIncome<- scale(employee_data$MonthlyIncome) 
employee_data$PercentSalaryHike<- scale(employee_data$PercentSalaryHike)
employee_data$TotalWorkingYears<- scale(employee_data$TotalWorkingYears)
employee_data$YearsAtCompany<- scale(employee_data$YearsAtCompany)
employee_data$YearsSinceLastPromotion<- scale(employee_data$YearsSinceLastPromotion)
employee_data$YearsWithCurrManager<- scale(employee_data$YearsWithCurrManager)
employee_data$avg_annual_hours<- scale(employee_data$avg_annual_hours) 

# Converting the target variable Attrition from No/Yes character to factor with levels 0/1 
employee_data$Attrition <- ifelse(employee_data$Attrition=="Yes",1,0)

# Now checking Attrition rate of the prospect employee

Attrition <- sum(employee_data$Attrition)/nrow(employee_data)
Attrition

#Attrition rate is 16.16%.

#Removing the two variables EmployeeCount and Over18 having single values
employee_data <- employee_data[,-c(14,21)]

# Subsetting the categorical variables to a dataframe
employee_data_Chr<- employee_data[,c(2,3,4,5,6,9,10,12,13,14,16,17)]

# Converting the categorical variables to factor
employee_data_fact<- data.frame(sapply(employee_data_Chr, function(x) factor(x)))

# Creating dummy variables for categorical columns
dummies<- data.frame(sapply(employee_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_data_fact))[,-1]))

# For variables having only two levels like in PerformanceRating "Outstanding" is 1,
#gender "male" is 1

# The Final dataset
employee_data_final<- cbind(employee_data[,-c(1,2,3,4,5,6,9,10,12,13,14,16,17)],dummies) 
View(employee_data_final) #4300 variables of 52 variables

############################################################################

#Splitting the data to create Train and Test data
set.seed(100)

indices <- sample.split(employee_data_final$Attrition, SplitRatio = 0.7)

train <- employee_data_final[indices,]
test <- employee_data_final[!(indices), ]

###################Logistic Regression Model Creation######################

#Initial model
model_1 <- glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC: 2113.3

#Step-wise selection
model_2 <- stepAIC(model_1, direction = "both")
summary(model_2) #AIC: 2092.2

vif(model_2)
#Removing EducationField.xLife.Sciences as this variable is having comparatively high VIF

model_3 <- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
              TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
              EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
              JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + WorkLifeBalance.xGood +
              JobInvolvement.xMedium + JobInvolvement.xVery.High + BusinessTravel.xTravel_Frequently +
              BusinessTravel.xTravel_Rarely + Education.xDoctor + EducationField.xMarketing + EducationField.xMedical +
              EducationField.xOther + EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
              JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobRole.xResearch.Scientist +
              JobRole.xSales.Executive  + MaritalStatus.xMarried + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_3) #AIC: 2107.6

vif(model_3)
#Removing BusinessTravel.xTravel_Frequently as this variable is having comparatively high VIF 

model_4 <- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + WorkLifeBalance.xGood +
                 JobInvolvement.xMedium + JobInvolvement.xVery.High  +
                 BusinessTravel.xTravel_Rarely + Education.xDoctor + EducationField.xMarketing + EducationField.xMedical +
                 EducationField.xOther + EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobRole.xResearch.Scientist +
                 JobRole.xSales.Executive  + MaritalStatus.xMarried + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_4) #AIC: 2135.9

vif(model_4)
#Removing WorkLifeBalance.xGood as this variable is having comparatively high VIF

model_5 <- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter +
                 JobInvolvement.xMedium + JobInvolvement.xVery.High  +
                 BusinessTravel.xTravel_Rarely + Education.xDoctor + EducationField.xMarketing + EducationField.xMedical +
                 EducationField.xOther + EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobRole.xResearch.Scientist +
                 JobRole.xSales.Executive  + MaritalStatus.xMarried + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_5) #AIC: 2152.9

vif(model_5)
#Almost all variables have low vif

#Removing EducationField.xMarketing as this variable is having high p value (low significance)

model_6 <- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter +
                 JobInvolvement.xMedium + JobInvolvement.xVery.High  +
                 BusinessTravel.xTravel_Rarely + Education.xDoctor  + EducationField.xMedical +
                 EducationField.xOther + EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobRole.xResearch.Scientist +
                 JobRole.xSales.Executive  + MaritalStatus.xMarried + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_6) #AIC: 2151.2

#Removing EducationField.xMedical as this variable is having high p value (low significance)

model_7 <- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter +
                 JobInvolvement.xMedium + JobInvolvement.xVery.High  +
                 BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                 EducationField.xOther + EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobRole.xResearch.Scientist +
                 JobRole.xSales.Executive  + MaritalStatus.xMarried + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_7) #AIC: 2150.1

#Removing EducationField.xOther as this variable is having high p value (low significance)

model_8 <- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter +
                 JobInvolvement.xMedium + JobInvolvement.xVery.High  +
                 BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + JobRole.xResearch.Scientist +
                 JobRole.xSales.Executive  + MaritalStatus.xMarried + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_8) #AIC: 2149.8

#Removing JobRole.xResearch.Scientist as this variable is having high p value (low significance)

model_9 <- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter +
                 JobInvolvement.xMedium + JobInvolvement.xVery.High  +
                 BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                 JobRole.xSales.Executive  + MaritalStatus.xMarried + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_9) #AIC: 2149.9

#Removing JobInvolvement.xVery.High as this variable is having high p value (low significance)

model_10 <- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter +
                  JobInvolvement.xMedium +
                  BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                  EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  JobRole.xSales.Executive  + MaritalStatus.xMarried + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_10) #AIC: 2150

#Removing JobInvolvement.xMedium as this variable is having high p value (low significance)

model_11 <- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter +
                  BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                  EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  JobRole.xSales.Executive  + MaritalStatus.xMarried + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_11) #AIC: 2149.7

#Removing MaritalStatus.xMarried as this variable is having high p value (low significance)

model_12 <- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter +
                  BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                  EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_12) #AIC: 2149.8

#Removing JobLevel as this variable is having high p value (low significance)

model_13 <- glm(Attrition ~ Age  + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter +
                  BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                  EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_13) #AIC: 2150.4

#Removing WorkLifeBalance.xBest as this variable is having high p value (low significance)

model_14 <- glm(Attrition ~ Age  + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High  + WorkLifeBalance.xBetter +
                  BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                  EducationField.xTechnical.Degree + JobRole.xHuman.Resources + JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_14) #AIC: 2150.8

#Removing EducationField.xTechnical.Degree as this variable is having high p value (low significance)

model_15 <- glm(Attrition ~ Age  + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High  + WorkLifeBalance.xBetter +
                  BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                  JobRole.xHuman.Resources + JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_15) #AIC: 2151.6

#Removing JobRole.xHuman.Resources as this variable is having high p value (low significance)

model_16 <- glm(Attrition ~ Age  + NumCompaniesWorked + StockOptionLevel + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High  + WorkLifeBalance.xBetter +
                  BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                  JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_16) #AIC: 2152.8

#Removing StockOptionLevel as this variable is having high p value (low significance)

model_17 <- glm(Attrition ~ Age  + NumCompaniesWorked  + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High  + WorkLifeBalance.xBetter +
                  BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                  JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_17) #AIC: 2153.5

#Removing JobRole.xSales.Executive as this variable is having high p value (low significance)

model_18 <- glm(Attrition ~ Age  + NumCompaniesWorked  + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High  + WorkLifeBalance.xBetter +
                  BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                  JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_18) #AIC: 2155.1

#Removing EnvironmentSatisfaction.xVery.High as this variable is having high p value (low significance)

model_19 <- glm(Attrition ~ Age  + NumCompaniesWorked  + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow  + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High  + WorkLifeBalance.xBetter +
                  BusinessTravel.xTravel_Rarely + Education.xDoctor   +
                  JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_19) #AIC: 2157

#Removing BusinessTravel.xTravel_Rarely as this variable is having high p value (low significance)

model_20 <- glm(Attrition ~ Age  + NumCompaniesWorked  + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow  + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High  + WorkLifeBalance.xBetter +
                  Education.xDoctor   +
                  JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_20) #AIC: 2158.6

#Removing Education.xDoctor as this variable is having comparatively high p value (low significance)

model_21 <- glm(Attrition ~ Age  + NumCompaniesWorked  + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow  + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High  + WorkLifeBalance.xBetter +
                  JobRole.xManager +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director  +
                  MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_21) #AIC: 2161.5

#Removing JobRole.xResearch.Director as this variable is having comparatively high p value (low significance)

model_22 <- glm(Attrition ~ Age  + NumCompaniesWorked  + TotalWorkingYears +
                  TrainingTimesLastYear + YearsSinceLastPromotion +YearsWithCurrManager + avg_annual_hours +
                  EnvironmentSatisfaction.xLow  + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High  + WorkLifeBalance.xBetter +
                  JobRole.xManager +
                  JobRole.xManufacturing.Director   +
                  MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_22) #AIC: 2164.1

########################################################################
# With 14 significant variables in the model

final_model<- model_22

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])


# Let's see the summary 

summary(test_pred)

# Add the prediction probability with the test data set for further model evaluation steps.
test$prob <- test_pred

# View the test dataset including prediction probabity.
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)


#install.packages("e1071")
#library(e1071)

#######################################################################

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#Accuracy : 85%
#Sensitivity : 26%       
#Specificity : 97%

# Here we can see accuaracy of the model is on higher side (85%) but sensitivity is very less.
# As per business scenario we have to predict Attrition rate hence Sensitivity need to be on
# on the higher side.

#######################################################################



#########################################################################################

# Let's find out the optimal probalility cutoff .


# Follwing is the reusable fuction which will be used to get performance based on cutoff
# value iteratively and return sensitivity, specificity and accuracy of the model.

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
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

# Call the perform_fn in a loop and assign the output toOUT matrix for "sensitivity", "specificity", "accuracy".
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


perf_plot <- plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) +
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
lines(s,OUT[,2],col="darkgreen",lwd=2) +
lines(s,OUT[,3],col=4,lwd=2) 

# Show performance plot.

perf_plot

# Add legends to the plot created earlier.

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy")) 

# Add a box over the plot.
box()

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.1776 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1776, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

# Get Accuracy, Sensitivity and Specificity from the confusion matrix using 
# cut-off value - 0.1776.
acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

# Show Accuracy
acc
# Accuracy : 0.7481

# Show Sensitivity
sens
# Sensitivity : 0.7416

# Show Specificity
spec
# Specificity : 0.7493

# So first, Attrition were assigned to all 1290 associates in the test data 
# set. For this, a probability cutoff of 0.5 was used. The model thus 
# made, was very accurate (Accuracy = 85%), but it had a very low sensitivity (26%).

# Hence we created functions and iteratively found out best cut-off value of 0.1776.
# Thus the accuracy of the model decreased a little but sensitivity increased to 74%.

# So our final model has very good performance in terms of sensitivity and Specificity.

##################################################################################################



### Lets find out KS -statistic - Test Data considering cut-off = 0.1776 ######

# Replace Yes/No with 1/0 correspondingly for Attrition attribute in test actual and predicted data.
test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)





#on testing  data get the kS-statistic measures.
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# Maximum ks- static value is way more than 40% which tells that the performance 
# of the model is really good.
# A high KS statistic means that not only does your model have all attrition
# at the top, it has all non-attrition at the bottom.


####################################################################
# Now evaluate the performance in term of Lift & Gain Chart 

# plotting the lift chart

# Creating the function to calculate gain and lift of the model.
lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=round(Cumresp/sum(totalresp)*100,2),
           Cumlift=round(Gain/(bucket*(100/groups)),2)) 
  return(gaintable)
}

# Calling the function to get gain, cumulative lift for each decile.
attrtion_decile = lift(test_actual_attrition, test_pred, groups = 10)

# Creating gain and lift graph and show in a grid for better visualization.
gain_plot <- ggplot(data=attrtion_decile, aes(x=bucket, y=Gain, group=1, label=Gain)) +
  geom_line(color="blue")+
  geom_point() +
  geom_text(aes(label=Gain),hjust=0.5, vjust=-1) 

lift_plot <- ggplot(data=attrtion_decile, aes(x=bucket, y=Cumlift, group=1, label=Cumlift)) +
  geom_line(color="cyan") +
  geom_point() +
  geom_text(aes(label=Cumlift),hjust=0.5, vjust=-1) 

# Showing plots in a grid.
plot_grid(gain_plot,lift_plot,nrow=1)

# The gain for the final model is 79.4% by the 4th decile, what it 
# basically means is that if we sort all according to probability, 
# then among the top 40% associates of this sorted list, we would find 79.3% 
# of all associates that were likely to resign from job.

# The lift just tells us the factor by which our final model is 
# outperforming a random model. For instance, in our model’s lift is 
# equal to 1.99 by the 4rd decile, it means that your model’s gain 
# by the end of the 4rd decile is 1.99 times that of a random model’s 
# gain at the end of 4 deciles. 
# In other words, our model catches 1.99 times more attrition than a 
# random model would have caught.


