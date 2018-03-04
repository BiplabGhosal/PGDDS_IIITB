#Loading required libraries
library(ggplot2)
library(dplyr)
library(lubridate)

#Reading the dataset
uber <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE, na.strings = c("","NA"))

#Checking for duplicate records if present
nrow(uber)  #6745
length(unique(uber$Request.id)) #6745
#Hence duplicate records are not present

#Changing Request and Drop Timestamp column to a fixed format using lubridate library

uber$Request.timestamp <- parse_date_time(uber$Request.timestamp, orders = c("%m/%d/%Y %H:%M","%d-%m-%Y %H:%M:%S"))
uber$Drop.timestamp <- parse_date_time(uber$Drop.timestamp, orders = c("%m/%d/%Y %H:%M","%d-%m-%Y %H:%M:%S"))

#Extracting the hour of the day from the requested time and storing in new column

uber$Request.Time.Hour <- format(uber$Request.timestamp, "%H")

#Creating time slots based on request time hour

#We can divide the Request time into 4 time slots.This can vary and can be changed as per business request
#23:00-04:00 - Late Night
#05:00-09:00 - Early Morning
#10:00-16:00 - Mid Day
#17:00-22:00 - Late Evening

#Adding a new col Time.Slot based on the requested time hour

for (i in 1:nrow(uber)) {
  sample <- uber[i, ]
  
  if(sample$Request.Time.Hour>='00' & sample$Request.Time.Hour <='04') {
    uber[i,"Time.Slot"] <- "Late Night"
  } else if (sample$Request.Time.Hour>='05' & sample$Request.Time.Hour<='09') {
    uber[i,"Time.Slot"] <- "Early Morning"
  } else if(sample$Request.Time.Hour>='10' & sample$Request.Time.Hour<='16') {
    uber[i,"Time.Slot"] <- "Mid Day"
  } else if(sample$Request.Time.Hour>='17' & sample$Request.Time.Hour <='23') {
    uber[i,"Time.Slot"] <- "Late Evening"
  }
}

#Visualizing the frequency of request and the different status based on the time slots derived
#Using the entire dataset consisting of trips from city to airport and from airport to city

ggplot(uber, aes(x=factor(Time.Slot),fill=Status)) + geom_bar(position = "dodge") +
  labs(title="Status of Request v/s Time Slot on all trips made") + labs(x="Time Slot", y="Count of request") +
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width = 1),vjust=-0.25)

#We can visulize that in Late Evening time slot most users gets status as "No cars available" and in 
#Early Morning time slot most users a good percentages of trips gets "cancelled"

#Analyzing further how the frequency of request and status varies with Pickup Point

#######For trips from city to airport#######

uber_city_pickup <- subset(uber, uber$Pickup.point=="City")

 ggplot(uber_city_pickup, aes(x=factor(Time.Slot),fill=Status)) + geom_bar(position = "dodge") +
  labs(title="Status of Request v/s Time Slot (For City to Airport trips)") + labs(x="Time Slot", y="Count of request") +
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width = 1),vjust=-0.25)

#We can visualize that for City to Airport trips huge number of users gets their trips "Cancelled" in the Early Morning Timeslot

#######For trips from airport to city#######

uber_airport_pickup <- subset(uber, uber$Pickup.point=="Airport")

ggplot(uber_airport_pickup, aes(x=factor(Time.Slot),fill=Status)) + geom_bar(position = "dodge") +
  labs(title="Status of Request v/s Time Slot (For Airport to City trips)") + labs(x="Time Slot", y="Count of request") +
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width = 1),vjust=-0.25) 

#We can visualize that for Airport to City trips huge number of users gets the status as "No Cars Available" in the Late Evening Timeslot

#######Visualizing on the entire dataset how request for pickup varies as per Time Slot#######

ggplot(uber, aes(x=factor(Time.Slot), fill=Pickup.point)) + geom_bar() +
  labs(title="Request of cabs as per time slots on all trips") + labs(x="Time Slots", y="Count of request") 
   
# From the above 3 plots we can visualize the following
#1. Request of pickup from Airport is maximum in the Late Evening and in the same time slot most users gets status as "No Cars Available"
#2. Request of pickup from City is maximum in the Early Morning and in the same time slot most users gets status as "Cancelled"

#############################################################################################################################################

#####Analyzing supply demand based on request types from city to airport or from airport to city#####

#Considering Airport pickup dataset and counting the demand, supply and gap for Airport to City trips

#Counting Demand and adding a new column

uber_airport_pickup <- 
  uber_airport_pickup %>%
  group_by(Request.Time.Hour) %>%
  mutate(demand=n()) %>%
  arrange((Request.Time.Hour))

#Counting Supply and adding a new column

uber_airport_pickup <- 
  uber_airport_pickup %>%
  group_by(Request.Time.Hour) %>%
  mutate(supply=length(which(Status=="Trip Completed"))) %>%
  arrange(Request.Time.Hour)

#Counting Gap and adding a new column

uber_airport_pickup <- 
  uber_airport_pickup %>%
  group_by(Request.Time.Hour) %>%
  mutate(gap=length(which(Status=="Cancelled" | Status=="No Cars Available"))) %>%
  arrange(Request.Time.Hour)

#Extracting Required columns for plotting and saving in a new dataset

uber_airport_demand_supply <- uber_airport_pickup[,c("Request.Time.Hour","Time.Slot","demand","supply","gap")]
uber_airport_demand_supply <- distinct(uber_airport_demand_supply)

#Plotting Supply Demand gap with respect to each hours of the day 

ggplot(uber_airport_demand_supply, aes(x=factor(Request.Time.Hour), y=gap, col=Time.Slot)) + geom_point(size=3) +
  labs(title="Supply Demand Gap in airport to city trips v/s Time Slots") + labs(x="Hours of the day", y="Gap count")

#So we can observe high gap in the Late Evening time slot, mainly between 5PM - 10PM.

#Analyzing this high gap in airport pickup during late evening time slot

length(which(uber_airport_pickup$Status=="Trip Completed" & uber_airport_pickup$Time.Slot=="Late Evening")) #515 
length(which(uber_airport_pickup$Status=="No Cars Available" & uber_airport_pickup$Time.Slot=="Late Evening")) #1457 
length(which(uber_airport_pickup$Status=="Cancelled" & uber_airport_pickup$Time.Slot=="Late Evening")) #109

#70% of users are not able to get cabs from airport (Status - No Cars Available) in late evening, resulting huge gap between demand and supply.


#Considering City pickup dataset and counting the demand, supply and gap for City to Airport trips

#Counting Demand and adding a new column

uber_city_pickup <- 
  uber_city_pickup %>%
  group_by(Request.Time.Hour) %>%
  mutate(demand=n()) %>%
  arrange((Request.Time.Hour))

#Counting Supply and adding a new column

uber_city_pickup <- 
  uber_city_pickup %>%
  group_by(Request.Time.Hour) %>%
  mutate(supply=length(which(Status=="Trip Completed"))) %>%
  arrange(Request.Time.Hour)

#Counting Gap and adding a new column

uber_city_pickup <- 
  uber_city_pickup %>%
  group_by(Request.Time.Hour) %>%
  mutate(gap=length(which(Status=="Cancelled" | Status=="No Cars Available"))) %>%
  arrange(Request.Time.Hour)

#Extracting Required columns for plotting and saving in a new dataset

uber_city_demand_supply <- uber_city_pickup[,c("Request.Time.Hour","Time.Slot","demand","supply","gap")]
uber_city_demand_supply <- distinct(uber_city_demand_supply)

#Plotting Supply Demand gap with respect to each hours of the day 

ggplot(uber_city_demand_supply, aes(x=factor(Request.Time.Hour), y=gap, col=Time.Slot)) + geom_point(size=3) +
  labs(title="Supply Demand Gap in city to airport trips v/s Time Slots") + labs(x="Hours of the day", y="Gap count")

#So we can observe high gap in the Early Morning time slot, mainly between 5AM - 9AM.

#Analyzing this high gap in city pickup during early morning time slot

length(which(uber_city_pickup$Status=="Trip Completed" & uber_airport_pickup$Time.Slot=="Early Morning")) #101 
length(which(uber_city_pickup$Status=="No Cars Available" & uber_airport_pickup$Time.Slot=="Early Morning")) #137 
length(which(uber_city_pickup$Status=="Cancelled" & uber_airport_pickup$Time.Slot=="Early Morning")) #204

#46% of users gets their request cancelled and and 30% of users get status as No Cars Available in early morning slot, resulting gap between demand and supply

#Comparing Demand Supply analysis of City and Airport Pickup

#Visualizing the Supply Demand plot in city and in airport, we can infer that
# Time slot when the highest gap exist in the entire day is between 5PM till 10PM (Late Evening Time Slot) for pickup requests from Airport. (airport-city)
# For city-airport requests the gap is most severe between 5AM - 9AM (Early Morning Time Slot) 
