setwd("F:/Work and Study/IIIT-B PGDDS/Main Courses/K Means Classification")

library(dplyr)

Online.Retail <- read.csv("Online Retail.csv", stringsAsFactors = FALSE)
sum(is.na(Online.Retail)) #135080
sum(is.na(Online.Retail$InvoiceNo)) #0
sum(is.na(Online.Retail$Description)) #0
sum(is.na(Online.Retail$Quantity)) #0
sum(is.na(Online.Retail$InvoiceDate))
sum(is.na(Online.Retail$UnitPrice))
sum(is.na(Online.Retail$CustomerID)) #135080

#Removing all records which are NA CustomerID, since these NA values cannot be replaced with other values

order_wise <- na.omit(Online.Retail)

#We need to cluster the customers present in the dataset. 
#We need to change the granularity of data such that each record corresponds to one customer id

#Recency - How recently visited
#Frequency - Frequency of transactions made
#Monetary - Total Amount spend

#Creating Amount based on order quantity and unit per price
order_wise$Amount <- order_wise$Quantity * order_wise$UnitPrice

#Sorting the dataset based on CustomerID
order_wise <- order_wise[order(order_wise$CustomerID), ]

#Aggregating based on CustomerID and creating Total amount spent based on each customer
monetary <- aggregate(Amount~CustomerID, order_wise, sum) #This vector is M in RFM

#Checking for frequency of purchase now. Can get his value by getting the count of sum of each customer id in the dataset
order_wise$CustomerID <- as.factor(order_wise$CustomerID)
frequency <- order_wise[,c(7,1)]
temp <- table(frequency$CustomerID)
temp <- data.frame(temp)
colnames(temp)[1] <- c("CustomerID") #This is our F in RFM

RFM <- merge(monetary, temp, by="CustomerID")

#Checking for recency now
recency <- order_wise[,c(7,5)]
recency$InvoiceDate <- as.Date(recency$InvoiceDate, "%m/%d/%Y %H:%M")

maximum <- max(recency$InvoiceDate)
maximum <- maximum + 1
maximum$diff <-maximum - recency$InvoiceDate
recency$diff<-maximum$diff
recency<-aggregate(recency$diff,by=list(recency$CustomerID),FUN="min")
colnames(recency)[1]<- "CustomerID"
colnames(recency)[2]<- "Recency"


RFM <- merge(RFM, recency, by = ("CustomerID"))
RFM$Recency <- as.numeric(RFM$Recency)


#Outlier treatment
box <- boxplot.stats(RFM$Amount)
out <- box$out
RFM1 <- RFM[ !RFM$Amount %in% out, ]
RFM <- RFM1
box <- boxplot.stats(RFM$Freq)
out <- box$out
RFM1 <- RFM[ !RFM$Freq %in% out, ]
RFM <- RFM1
box <- boxplot.stats(RFM$Recency)
out <- box$out
RFM1 <- RFM[ !RFM$Recency %in% out, ]
RFM <- RFM1

#Scaling of variables

RFM_norm1 <- RFM[,-1]

RFM_norm1$Amount <- scale(RFM_norm1$Amount)
RFM_norm1$Freq <- scale(RFM_norm1$Freq)
RFM_norm1$Recency <- scale(RFM_norm1$Recency)

## Implementing K-Means algorithm

clus3 <- kmeans(RFM_norm1, centers = 3, iter.max = 50, nstart = 50)
str(clus3) 
  
## Finding the optimal value of K

r_sq<- rnorm(20)

for (number in 1:20){clus <- kmeans(RFM_norm1, centers = number, nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss
}

plot(r_sq)
#Based on elbow method we can get the value of K

## Running the K-Means algorithm for K =4,5,6

clus4 <- kmeans(RFM_norm1, centers = 4, iter.max = 50, nstart = 50)

clus5 <- kmeans(RFM_norm1, centers = 5, iter.max = 50, nstart = 50)

clus6 <- kmeans(RFM_norm1, centers = 6, iter.max = 50, nstart = 50)

## Appending the ClusterIDs to RFM data
RFM_km <-cbind(RFM,clus5$cluster)

colnames(RFM_km)[5]<- "ClusterID"

## Cluster Analysis. Grouping based on clusters and calculation mean amount, freq and recency

library(dplyr)

km_clusters<- group_by(RFM_km, ClusterID)

tab1<- summarise(km_clusters, Mean_amount=mean(Amount), Mean_freq=mean(Freq), Mean_recency=mean(Recency))

#Visualizing the clusters
library(ggplot2)

ggplot(tab1, aes(x= factor(ClusterID), y=Mean_amount)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(ClusterID), y=Mean_freq)) + geom_bar(stat = "identity")
ggplot(tab1, aes(x= factor(ClusterID), y=Mean_recency)) + geom_bar(stat = "identity")
