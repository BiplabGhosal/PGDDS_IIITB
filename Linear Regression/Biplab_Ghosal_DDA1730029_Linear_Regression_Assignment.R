#Load essential libraries
library(stringr)
library(tidyr)
library(ggplot2)
library(MASS)
library(car)

######################Data Loading######################

car <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE, na.strings = c("","NA"))
View(car)

#Checking for NA values
sum(is.na(car)) #No NA values present in the dataset

#Checking for unique records
nrow(car)
unique(nrow(car)) #No duplicate records present

#Checking structure of dataframe
str(car)

#Checking summary of car dataset
summary(car)

######################Data Cleaning and Data Preparation######################

#Deriving company name from CarName variable
car <- separate(car, CarName, into=c("CarCompany", "CarModel"), sep=" ", extra = "merge", fill = "left", drop="FALSE")

table(car$CarCompany) #There are many spelling mistakes in the Car company names. Making necessary corrections 

car$CarCompany[which(car$CarCompany=="alfa-romero")] <- 'alfa-romeo'
car$CarCompany[which(car$CarCompany=="maxda")] <- 'mazda'
car$CarCompany[which(car$CarCompany=="Nissan")] <- 'nissan'
car$CarCompany[which(car$CarCompany=="porcshce")] <- 'porsche'
car$CarCompany[which(car$CarCompany=="toyouta")] <- 'toyota'
car$CarCompany[which(car$CarCompany=="vokswagen")] <- 'volkswagen'
car$CarCompany[which(car$CarCompany=="vw")] <- 'volkswagen'

# Updating Company Name for CarId 139 and 142. These are subaru cars with no car model number
car$CarCompany[139] <- "subaru"
car$CarCompany[142] <- "subaru"

#Removing car model column as this is not required for analysis
car <- car[,-c(4)]

##Converting categorical variables to Factor Data Type##

#Converting Symboling variable as factor 
car$symboling <- as.factor(car$symboling)

#Converting CarCompany variable as factor
car$CarCompany <- as.factor(car$CarCompany)

#Converting FuelType variable as factor
car$fueltype <- as.factor(car$fueltype)

#Converting Aspiration variable as factor
car$aspiration <- as.factor(car$aspiration)

#Converting doornumber variable as factor
car$doornumber <- as.factor(car$doornumber)

#Converting carbody variable as factor
car$carbody <- as.factor(car$carbody)

#Converting drivewheel variable as factor
car$drivewheel <- as.factor(car$drivewheel)

#Converting Engine Location as factor
car$enginelocation <- as.factor(car$enginelocation)

#Converting EngineType as factor
car$enginetype <- as.factor(car$enginetype)

#Converting cylindernumber as factor
car$cylindernumber <- as.factor(car$cylindernumber)

#Converting fuelsystem as factor
car$fuelsystem <- as.factor(car$fuelsystem)

##Outlier treatment of numeric variables##

#wheelbase variable
quantile(car$wheelbase,seq(0,1,0.01)) #No such significant jump present

#carlength variable
quantile(car$carlength, seq(0,1,0.01)) #No such significant jump present

#carwidth variable
quantile(car$carwidth, seq(0,1,0.01)) #No such significant jump present

#carheigth variable
quantile(car$carheight, seq(0,1,0.01)) #No such significant jump present

#curbweigth variable
quantile(car$curbweight, seq(0,1,0.01)) #No such significant jump present

#enginesize variable
quantile(car$enginesize,seq(0,1,0.01)) 
#Here after 96th percentile the value jumps suddenly from 209 to 234. Then goes up till 326.
#Taking 209 as the limit, we would mark all values above 209 to 209.
car$enginesize[which(car$enginesize>209)] <- 209

#boreratio variable
quantile(car$boreratio, seq(0,1,0.01)) #No such significant jump present

#stroke variable
quantile(car$stroke, seq(0,1,0.01)) #No such significant jump present

#compressionratio variable
quantile(car$compressionratio, seq(0,1,0.01))
#Here after 90th percentile the value jumps suddenly from 10.9400 to 21.00. Then goes up till 23.00
#Taking 10.9400 as the limit, we would mark all values above 10.9400 to 10.9400
car$compressionratio[which(car$compressionratio>10.9400)] <- 10.9400

#horsepower variable
quantile(car$horsepower, seq(0,1,0.01))
#Here after 97th percentile the value jumps suddenly from 184.00 to 207.00. Then goes up till 288.00
#Taking 184.00 as the limit, we would mark all values above 184.00 to 184.00
car$horsepower[which(car$horsepower>184.00)] <- 184.00

#peakrpm variable
quantile(car$peakrpm, seq(0,1,0.01))
#Here after 99th percentile the value jumps suddenly from 6000.00 to 6600.0
#Taking 6000.00 as the limit, we would mark all values above 6000.00 to 6000.00
car$peakrpm[which(car$peakrpm>6000.0)] <- 6000.0

#citympg variable
quantile(car$citympg, seq(0,1,0.01))
#Here after 98th percentile the value jumps suddenly from 38.00 to 44.72. Then goes up till 49.00
#Taking 38.00 as the limit, we would mark all values above 38.00 to 38.00
car$citympg[which(car$citympg>38.00)] <- 38.00

#highwaympg variable
quantile(car$highwaympg, seq(0,1,0.01)) #No such significant jump present


###################Univariate Analysis through EDA ####################

#Plotting categorical variables to get an overview of the distribution

#Symboling variable
G1 <- ggplot(car, aes(x=car$symboling)) + geom_bar(fill="blue") +
  labs(title="Symboling Distribution", x = "Symboling", y="Count")
G1 

#CarCompany variable
G2 <- ggplot(car, aes(x=car$CarCompany)) + geom_bar(fill="gray") +
  labs(title="Car Company Distribution", x = "Car Company", y="Count")
G2

#FuelType variable
G3 <- ggplot(car, aes(x=car$fueltype)) + geom_bar(fill="darkcyan") + 
  labs(title="Fuel Type Distribution", x="Fuel type", y="Count")
G3

#Aspiration variable
G4 <- ggplot(car, aes(x=car$aspiration)) + geom_bar(fill="deepskyblue") +
  labs(title="Aspiration Distribution", x="Aspiration", y="Count")
G4

#DoorNumber variable
G5 <- ggplot(car, aes(x=car$doornumber)) + geom_bar(fill="coral4") +
  labs(title="DoorNumber Distribution", x="DoorNumber", y="Count")
G5

#CarBody variable
G6 <- ggplot(car, aes(x=car$carbody)) + geom_bar(fill="palegreen4") +
  labs(title="CarBody Distribution", x="CarBody Type", y="Count")
G6

#Drivewheel variable
G7 <- ggplot(car, aes(x=car$drivewheel)) + geom_bar(fill="brown4") +
  labs(title="Drivewheel Distribution", x="Drivewheel Type", y="Count")
G7

#EngineLocation variable
G8 <- ggplot(car, aes(x=car$enginelocation)) + geom_bar(fill="yellow4") +
  labs(title="Engine Location Distribution", x="Engine Location Type", y="Count")
G8

#EngineType variable
G9 <- ggplot(car, aes(x=car$enginetype)) + geom_bar(fill="orchid") +
  labs(title="Engine Type Distribution", x="Engine Type", y="Count")
G9

#CylinderNumber variable
G10 <- ggplot(car, aes(x=car$cylindernumber)) + geom_bar(fill="sienna4") +
  labs(title="Cylinder Number Distribution", x="Cylinder Number", y="Count")
G10

#FuelSystem variable
G11 <- ggplot(car, aes(x=car$fuelsystem)) + geom_bar(fill="violet") +
  labs(title="Fuel System Distribution", x="Fuel System", y="Count")
G11

#We are good to proceed with dummy variable creation


####################Dummy Variable creation####################

car_1 <- car #Taking backup

#First considering only variables which have 2 levels

#fueltype variable
levels(car_1$fueltype) <- c(1,0)
car_1$fueltype <- as.numeric(levels(car_1$fueltype))[car_1$fueltype]

#aspiration variable
levels(car_1$aspiration) <- c(1,0)
car_1$aspiration <- as.numeric(levels(car_1$aspiration))[car_1$aspiration]

#doornumber variable
levels(car_1$doornumber) <- c(1,0)
car_1$doornumber <- as.numeric(levels(car_1$doornumber))[car_1$doornumber]

#enginelocation variable
levels(car_1$enginelocation) <- c(1,0)
car_1$enginelocation <- as.numeric(levels(car_1$enginelocation))[car_1$enginelocation]

#Now considering multilevel variables

#carcompany variable
dummy_carcompany <- model.matrix(~CarCompany, data = car_1)
dummy_carcompany <- dummy_carcompany[, -1]
#Combining dummy_carcompany with main dataset after removing the original categorical CarCompany variable
car_1 <- cbind(car_1[,-3], dummy_carcompany)

#carbody variable
dummy_carbody <- model.matrix(~carbody, data = car_1)
dummy_carbody <- dummy_carbody[, -1]
#Combining dummy_carbody with main dataset after removing the original categorical carbody variable
car_1 <- cbind(car_1[,-6], dummy_carbody)

#drivewheel variable
dummy_drivewheel <- model.matrix(~drivewheel, data = car_1)
dummy_drivewheel <- dummy_drivewheel[, -1]
#Combining dummy_drivewheel with main dataset after removing the original categorical drivewheel variable
car_1 <- cbind(car_1[,-6], dummy_drivewheel)

#enginetype variable
dummy_enginetype <- model.matrix(~enginetype, data = car_1)
dummy_enginetype <- dummy_enginetype[, -1]
#Combining dummy_enginetype with main dataset after removing the original categorical enginetype variable
car_1 <- cbind(car_1[,-12], dummy_enginetype)

#cylindernumber variable
dummy_cylindernumber <- model.matrix(~cylindernumber, data = car_1)
dummy_cylindernumber <- dummy_cylindernumber[, -1]
#Combining dummy_cylindernumber with main dataset after removing the original categorical cylindernumber variable
car_1 <- cbind(car_1[, -12], dummy_cylindernumber)

#fuelsystem variable
dummy_fuelsystem <- model.matrix(~fuelsystem, data = car_1)
dummy_fuelsystem <- dummy_fuelsystem[, -1]
#Combining dummy_fuelsystem with main dataset after removing the original categorical fuelsystem variable
car_1 <- cbind(car_1[,-13], dummy_fuelsystem)

#symboling variable
dummy_symboling <- model.matrix(~symboling, data = car_1)
dummy_symboling <- dummy_symboling[, -1]
#Combining dummy_symboling with main dataset after removing the original categorical symboling variable
car_1 <- cbind(car_1[,-2], dummy_symboling)


########################Model Creation#####################

#Correlation check between variables

cor_check <- cor(car_1)
write.csv(cor_check, "correlation_check.csv") 
#Some variables are highly correlated, we can remove them subsequently

#Dividing training and test data
set.seed(999)
trainindices <- sample(1:nrow(car_1), 0.7*nrow(car_1))
train.car <- car_1[trainindices, ]  #Training set
test.car <- car_1 [-trainindices, ] #Test set

#Model1 consisting of all independent variables
model1 <- lm(price ~ ., data = train.car)
summary(model1) #R-squared:  0.9658,	Adjusted R-squared:  0.94

#Using stepAIC function
step <- stepAIC(model1, direction = "both")

#Model2 consisting of variables based on stepAIC selection

model2 <- lm(price ~ stroke + fuelsystemmfi + drivewheelrwd + fuelsystem4bbl +
               fuelsystemspdi + CarCompanysaab + boreratio + CarCompanyjaguar +
               symboling0 + highwaympg + enginetypeohcv + car_ID + CarCompanybmw +
               cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
               aspiration + carbodyhardtop + fuelsystemmpfi + CarCompanyvolvo +
               CarCompanysubaru + peakrpm + carwidth + CarCompanytoyota + CarCompanyrenault +
               compressionratio + enginetypel + CarCompanyporsche + carbodyhatchback +
               CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
               curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
               CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
               enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model2) #R-squared:  0.9637,	Adjusted R-squared:  0.9463

#Check for multicollinearity

vif(model2)

#Removing car_ID as this variable has highest VIF value and is also having not significance (high p value)

model3 <- lm(price ~ stroke + fuelsystemmfi + drivewheelrwd + fuelsystem4bbl +
               fuelsystemspdi + CarCompanysaab + boreratio + CarCompanyjaguar +
               symboling0 + highwaympg + enginetypeohcv  + CarCompanybmw +
               cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
               aspiration + carbodyhardtop + fuelsystemmpfi + CarCompanyvolvo +
               CarCompanysubaru + peakrpm + carwidth + CarCompanytoyota + CarCompanyrenault +
               compressionratio + enginetypel + CarCompanyporsche + carbodyhatchback +
               CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
               curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
               CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
               enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model3) #R-squared:  0.9624,	Adjusted R-squared:  0.9449 

#check for multicollinearity

vif(model3)

#Removing CarCompanysaab as this variable has high p value and moderately high VIF
#Not removing other high VIF values variables, as those are highly significant (extremely low p value)

model4 <- lm(price ~ stroke + fuelsystemmfi + drivewheelrwd + fuelsystem4bbl +
               fuelsystemspdi  + boreratio + CarCompanyjaguar +
               symboling0 + highwaympg + enginetypeohcv  + CarCompanybmw +
               cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
               aspiration + carbodyhardtop + fuelsystemmpfi + CarCompanyvolvo +
               CarCompanysubaru + peakrpm + carwidth + CarCompanytoyota + CarCompanyrenault +
               compressionratio + enginetypel + CarCompanyporsche + carbodyhatchback +
               CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
               curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
               CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
               enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model4) #R-squared:  0.9623,	Adjusted R-squared:  0.9454 

vif(model4)

#Removing highwaympg as this variable has high vif and also has high p value

model5 <- lm(price ~ stroke + fuelsystemmfi + drivewheelrwd + fuelsystem4bbl +
               fuelsystemspdi  + boreratio + CarCompanyjaguar +
               symboling0  + enginetypeohcv  + CarCompanybmw +
               cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
               aspiration + carbodyhardtop + fuelsystemmpfi + CarCompanyvolvo +
               CarCompanysubaru + peakrpm + carwidth + CarCompanytoyota + CarCompanyrenault +
               compressionratio + enginetypel + CarCompanyporsche + carbodyhatchback +
               CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
               curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
               CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
               enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)


summary(model5) #R-squared:  0.9615,	Adjusted R-squared:  0.9448

vif(model5)

#Removing CarCompanyjaguar as this variable is having very high p value

model6 <- lm(price ~ stroke + fuelsystemmfi + drivewheelrwd + fuelsystem4bbl +
               fuelsystemspdi  + boreratio  +
               symboling0  + enginetypeohcv  + CarCompanybmw +
               cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
               aspiration + carbodyhardtop + fuelsystemmpfi + CarCompanyvolvo +
               CarCompanysubaru + peakrpm + carwidth + CarCompanytoyota + CarCompanyrenault +
               compressionratio + enginetypel + CarCompanyporsche + carbodyhatchback +
               CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
               curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
               CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
               enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model6) #R-squared:  0.9608,	Adjusted R-squared:  0.9444

vif(model6)

#Removing boreratio as this variable is having high VIF and comparatively high p value

model7 <- lm(price ~ stroke + fuelsystemmfi + drivewheelrwd + fuelsystem4bbl +
               fuelsystemspdi   +
               symboling0  + enginetypeohcv  + CarCompanybmw +
               cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
               aspiration + carbodyhardtop + fuelsystemmpfi + CarCompanyvolvo +
               CarCompanysubaru + peakrpm + carwidth + CarCompanytoyota + CarCompanyrenault +
               compressionratio + enginetypel + CarCompanyporsche + carbodyhatchback +
               CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
               curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
               CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
               enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model7) #R-squared:  0.959,	Adjusted R-squared:  0.9423

vif(model7)

#Removing fuelsystemspdi as this variable is having very high p value

model8 <- lm(price ~ stroke + fuelsystemmfi + drivewheelrwd + fuelsystem4bbl +
               symboling0  + enginetypeohcv  + CarCompanybmw +
               cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
               aspiration + carbodyhardtop + fuelsystemmpfi + CarCompanyvolvo +
               CarCompanysubaru + peakrpm + carwidth + CarCompanytoyota + CarCompanyrenault +
               compressionratio + enginetypel + CarCompanyporsche + carbodyhatchback +
               CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
               curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
               CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
               enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model8) #R-squared:  0.958,	Adjusted R-squared:  0.9415 

vif(model8)

#Removing fuelsystemmfi as this variable is having high p value

model9 <- lm(price ~ stroke + drivewheelrwd + fuelsystem4bbl +
               symboling0  + enginetypeohcv  + CarCompanybmw +
               cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
               aspiration + carbodyhardtop + fuelsystemmpfi + CarCompanyvolvo +
               CarCompanysubaru + peakrpm + carwidth + CarCompanytoyota + CarCompanyrenault +
               compressionratio + enginetypel + CarCompanyporsche + carbodyhatchback +
               CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
               curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
               CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
               enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model9) #R-squared:  0.9573,	Adjusted R-squared:  0.9412

#Removing fuelsystem4bbl as this variable is having high p value

model10 <- lm(price ~ stroke + drivewheelrwd  +
                symboling0  + enginetypeohcv  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
                aspiration + carbodyhardtop + fuelsystemmpfi + CarCompanyvolvo +
                CarCompanysubaru + peakrpm + carwidth + CarCompanytoyota + CarCompanyrenault +
                compressionratio + enginetypel + CarCompanyporsche + carbodyhatchback +
                CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model10) #R-squared:  0.9563,	Adjusted R-squared:  0.9404

#Removing peakrpm as this variable is having high p value

model11 <- lm(price ~ stroke + drivewheelrwd  +
                symboling0  + enginetypeohcv  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
                aspiration + carbodyhardtop + fuelsystemmpfi + CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                compressionratio + enginetypel + CarCompanyporsche + carbodyhatchback +
                CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model11) #R-squared:  0.9554,	Adjusted R-squared:  0.9397

#Removing compressionratio as this variable is having high p value

model12 <- lm(price ~ stroke + drivewheelrwd  +
                symboling0  + enginetypeohcv  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
                aspiration + carbodyhardtop + fuelsystemmpfi + CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel + CarCompanyporsche + carbodyhatchback +
                CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)


summary(model12) #R-squared:  0.9546,	Adjusted R-squared:  0.9392

#Removing fuelsystemmpfi as this variable is having high p value

model13 <- lm(price ~ stroke + drivewheelrwd  +
                symboling0  + enginetypeohcv  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
                aspiration + carbodyhardtop  + CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel + CarCompanyporsche + carbodyhatchback +
                CarCompanyisuzu + wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model13) #R-squared:  0.9542,	Adjusted R-squared:  0.9393

vif(model13)

#Removing CarCompanyisuzu as this variable is having high p value

model14 <- lm(price ~ stroke + drivewheelrwd  +
                symboling0  + enginetypeohcv  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen + carheight +
                aspiration + carbodyhardtop  + CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel + CarCompanyporsche + carbodyhatchback +
                wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model14) #R-squared:  0.9528,	Adjusted R-squared:  0.9379 

vif(model14)

#Removing carheight as this variable is having high p value

model15 <- lm(price ~ stroke + drivewheelrwd  +
                symboling0  + enginetypeohcv  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen +
                aspiration + carbodyhardtop  + CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel + CarCompanyporsche + carbodyhatchback +
                wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model15) #R-squared:  0.9517,	Adjusted R-squared:  0.937

vif(model15)

#Removing enginetypeohcv as this variable is having comparatively high p value

model16 <- lm(price ~ stroke + drivewheelrwd  +
                symboling0  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen +
                aspiration + carbodyhardtop  + CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel + CarCompanyporsche + carbodyhatchback +
                wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model16) #R-squared:  0.9494,	Adjusted R-squared:  0.9347

#Removing carbodyhatchback as this variable is having comparatively high p value

model17 <- lm(price ~ stroke + drivewheelrwd  +
                symboling0  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen +
                aspiration + carbodyhardtop  + CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel + CarCompanyporsche  +
                wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)


summary(model17) #R-squared:  0.9473,	Adjusted R-squared:  0.9325

#Removing aspiration as this variable is having comparatively high p value

model18 <- lm(price ~ stroke + drivewheelrwd  +
                symboling0  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen +
                carbodyhardtop  + CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel + CarCompanyporsche  +
                wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model18) #R-squared:  0.9447,	Adjusted R-squared:  0.9299

#Removing stroke as this variable is having high p value

model19 <- lm(price ~ drivewheelrwd  +
                symboling0  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen +
                carbodyhardtop  + CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel + CarCompanyporsche  +
                wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model19) #R-squared:  0.9437,	Adjusted R-squared:  0.9293 

#Removing carbodyhardtop as this variable is having high p value

model20 <- lm(price ~ drivewheelrwd  +
                symboling0  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel + CarCompanyporsche  +
                wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model20) #R-squared:  0.9415,	Adjusted R-squared:  0.9271

#Removing CarCompanyporsche as this variable is having high p value

model21 <- lm(price ~ drivewheelrwd  +
                symboling0  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel +
                wheelbase + CarCompanynissan + enginetyperotor + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model21) #R-squared:  0.9388,	Adjusted R-squared:  0.9244

#Removing enginetyperotor as this variable is having high p value

model22 <- lm(price ~ drivewheelrwd  +
                symboling0  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel +
                wheelbase + CarCompanynissan  + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + enginesize + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model22) #R-squared:  0.9368,	Adjusted R-squared:  0.9226

vif(model22)

#Removing enginesize as this variable is having high p variable

model23 <- lm(price ~ drivewheelrwd  +
                symboling0  + CarCompanybmw +
                cylindernumbersix + symboling1 + CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel +
                wheelbase + CarCompanynissan  + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi  + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model23) #R-squared:  0.936,	Adjusted R-squared:  0.9223 

#Removing symboling1 as this variable is having high p value

model24 <- lm(price ~ drivewheelrwd  +
                symboling0  + CarCompanybmw +
                cylindernumbersix  + CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel +
                wheelbase + CarCompanynissan  + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi  + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model24) #R-squared:  0.9339,	Adjusted R-squared:  0.9205 

#Removing symboling0 as this variable is having high p value

model25 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                cylindernumbersix  + CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel +
                wheelbase + CarCompanynissan  + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi  + CarCompanychevrolet + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model25) #R-squared:  0.9323,	Adjusted R-squared:  0.9192

#Removing CarCompanychevrolet as this variable is having high p value

model26 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                cylindernumbersix  + CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                enginetypel +
                wheelbase + CarCompanynissan  + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model26) #R-squared:  0.9304,	Adjusted R-squared:  0.9177

#Removing enginetypel as this variable is having high p value

model27 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                cylindernumbersix  + CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                wheelbase + CarCompanynissan  + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model27) #R-squared:  0.9297,	Adjusted R-squared:  0.9175

#Removing cylindernumbersix as this variable is having high p value

model28 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                wheelbase + CarCompanynissan  + carlength +
                curbweight + CarCompanyplymouth + cylindernumberfive + CarCompanymazda + 
                CarCompanymitsubishi + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model28) #R-squared:  0.9274,	Adjusted R-squared:  0.9155

#Removing cylindernumberfive as this variable is having high p value

model29 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                wheelbase + CarCompanynissan  + carlength +
                curbweight + CarCompanyplymouth + CarCompanymazda + 
                CarCompanymitsubishi + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model29) #R-squared:  0.9244,	Adjusted R-squared:  0.9127

#Removing wheelbase as this variable is having high p value

model30 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                CarCompanynissan  + carlength +
                curbweight + CarCompanyplymouth + CarCompanymazda + 
                CarCompanymitsubishi + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model30) #R-squared:  0.9215,	Adjusted R-squared:  0.9101

#Removing CarCompanyplymouth as this variable is having high p value

model31 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                CarCompanynissan  + carlength +
                curbweight + CarCompanymazda + 
                CarCompanymitsubishi + CarCompanyhonda +
                enginelocation + CarCompanydodge + CarCompanypeugeot, data = train.car)

summary(model31) #R-squared:  0.9175,	Adjusted R-squared:  0.9063

#Removing CarCompanydodge as this variable is having high p value

model32 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                CarCompanynissan  + carlength +
                curbweight + CarCompanymazda + 
                CarCompanymitsubishi + CarCompanyhonda +
                enginelocation  + CarCompanypeugeot, data = train.car)

summary(model32) #R-squared:  0.9146,	Adjusted R-squared:  0.9038

#Removing CarCompanyhonda as this variable is having high p value

model33 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                CarCompanyvolkswagen +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                CarCompanynissan  + carlength +
                curbweight + CarCompanymazda + 
                CarCompanymitsubishi  +
                enginelocation  + CarCompanypeugeot, data = train.car)

summary(model33) #R-squared:  0.9124,	Adjusted R-squared:  0.9021

#Removing CarCompanyvolkswagen as this variable is having high p value

model34 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                CarCompanynissan  + carlength +
                curbweight + CarCompanymazda + 
                CarCompanymitsubishi  +
                enginelocation  + CarCompanypeugeot, data = train.car)

summary(model34) #R-squared:  0.9107,	Adjusted R-squared:  0.901

#Removing CarCompanymitsubishi as this variable is having high p value

model34 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                CarCompanynissan  + carlength +
                curbweight + CarCompanymazda + 
                enginelocation  + CarCompanypeugeot, data = train.car)

summary(model34) #R-squared:  0.9092,	Adjusted R-squared:  0.9001

#Removing carlength as this variable is having high p value

model35 <- lm(price ~ drivewheelrwd  +
                CarCompanybmw +
                CarCompanyvolvo +
                CarCompanysubaru  + carwidth + CarCompanytoyota + CarCompanyrenault +
                CarCompanynissan   +
                curbweight + CarCompanymazda + 
                enginelocation  + CarCompanypeugeot, data = train.car)

summary(model35) #R-squared:  0.9067,	Adjusted R-squared:  0.8981


#model35 has only significant parameters (considering p value should be less than 0.05 )
#so we can use this model for prediction

#Predicting using the test dataset

predict_1 <- predict(model35, test.car[,-19])
test.car$test_price <- predict_1

#Checking accuracy of the predictions
r <- cor(test.car$price,test.car$test_price)

#R squared
rsquared <- cor(test.car$price,test.car$test_price)^2
rsquared

#Plotting actual data and predicted data

plot(test.car$price, col="red")
lines(test.car$test_price, col="blue")

#Identified the following variables are most significant for predicting the price of cars
#drivewheelrwd
#CarCompanybmw
#CarCompanyvolvo
#CarCompanysubaru
#carwidth
#CarCompanytoyota
#CarCompanyrenault
#CarCompanynissan
#curbweight
#CarCompanymazda
#enginelocation
#CarCompanypeugeot
