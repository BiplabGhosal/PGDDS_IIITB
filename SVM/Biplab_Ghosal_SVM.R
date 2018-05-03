############################ SVM Digit Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation
#####################################################################################

#####################################################################################
# 1. Business Understanding: 

#The objective is to identify each of a handwritten digits based on pixel values
#####################################################################################

#####################################################################################
# 2. Data Understanding: 
#MNIST data which is a large database of handwritten digits where we have pixel values 
#of each digit along with its label.

#Training dataset contains 60000 records
#Test dataset contains 10000 records
#####################################################################################

#####################################################################################
# 3. Data Preparation:

#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(dplyr)
library(caTools)

#Loading data
train <- read.delim('mnist_train.csv', sep = ',', stringsAsFactors = FALSE, header = FALSE)
test <- read.delim('mnist_test.csv', sep = ',', stringsAsFactors = FALSE, header = FALSE)

colnames(train)[1] <- 'number'
colnames(test)[1] <- 'number'

#Adding a new column signifying train or test data in the dataframe
train$type <- 'train'
test$type <- 'test'

#Combining train and test dataset
combined <- rbind(train,test)

#Checking for NA values in the combined dataset
which(sapply(combined, function(x) sum(is.na(x)))!=0) #No NA values

#Checking dimensions and structure of combined dataset
str(combined)
dim(combined)

#Removing columns which have all zeroes and having few number of non zero values
cols <- nearZeroVar(combined, freqCut = 95/5, names = TRUE, uniqueCut = 5)
combined <- combined[, !(names(combined)%in%cols)]

#Getting back training and test data
train_2 <- combined %>% filter(type == 'train') %>% select(-type)
test_2 <- combined %>% filter(type == 'test')  %>% select(-type)

#Converting number column to factor datatype
train_2$number <- as.factor(train_2$number)
test_2$number <- as.factor(test_2$number)

#Since train dataset is huge, we will sample out some data for training of the model

set.seed(100)
train_sample_indices <- sample.split(train_2$number, SplitRatio = 0.10)
training_set <- train_2[train_sample_indices,]
#nrow(training_set) #5999
test_set <- test_2

#####################################################################################

#####################################################################################
#4. Model Building

#Lets build a standard linear SVM model first with default parameters
linear_model_1 <- ksvm(number ~ ., data = training_set, kernel = 'vanilladot', C=1, scale = FALSE)

#Checking model accuracy (linear_model_1)
evaluate_linear_model_1 <- predict(linear_model_1, test_set)
confusionMatrix(evaluate_linear_model_1, test_set$number) #Accuracy : 0.8818

#Now lets create a RBF kernel model with default parameters and see is accuracy improves
RBF_model_1 <- ksvm(number ~ ., data = training_set, kernel = 'rbfdot', scale = FALSE)

#Checking model accuracy (RBF_model_1)
evaluate_RBF_model_1 <- predict(RBF_model_1, test_set)
confusionMatrix(evaluate_RBF_model_1, test_set$number) #Accuracy : 0.9492

#So RBF model is performing better than linear model with default parameters
#We should try building the cross validation based on this.

#Considering number of folds = 2
trainControl <- trainControl(method = "cv", number = 2)
metric <- 'Accuracy'

set.seed(7)

grid <- expand.grid(.sigma=c(0.0025, 0.0050, 0.010), .C=c(0.1,0.5, 1,2))
svm.fit <- train(number ~., data = training_set, method = "svmRadial", metric = metric, 
                 tuneGrid = grid, trContorl = trainControl)

#Considering number of folds and C values based on the time taken to run the svm.fit

#Plotting and printing the svm.fit to get the best tune sigma and C value
print(svm.fit)
plot(svm.fit)

#For sigma = 0.0050 & C=2, Accuracy = 0.9521

#Validating the model results on test data and checking over fitting
evaluate_non_linear <- predict(svm.fit, test_set)
confusionMatrix(evaluate_non_linear, test_set$number)
# Accuracy = 0.9608

#Clearly the model has been able to predict the test data with more than train model accuracy
