
library(data.table)
mydat <- fread('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv')
head(mydat)
str(mydat)
write.csv(mydat, file = "greenTaxi.csv", row.names = FALSE)
summary(mydat)
# Prepare Data
mydata <- na.omit(mydat) # listwise deletion of missing
mydata <- scale(mydat) # standardize variables
write.csv(mydata, file = "greenTaxicleanData.csv", row.names = FALSE)
mydata
is.data.frame(mydat)
hist(mydat$Trip_distance, breaks = 50, xlab = "Trip Distance", col = "gray", main = "Number of Trip Distance" )
hist(mydat$Trip_distance, main = "Number of Trip Distance", xlab = "Trip Distance", 
     col="darkgreen")
hist(mydat$VendorID, main = "Number of vendor", xlab = "vendor", 
     col="darkgreen")
barplot(table(mydat$VendorID),main = "Distribution of vendor id" , xlab = "vendor Id")
barplot(table(mydat$RateCodeID), main = "Distribution of Rate code id" , xlab = "Rate code Id")
barplot(table(mydat$Passenger_count),main = "Distribution of passenger count" , xlab = "passenger count")
barplot(table(mydat$Trip_type),main = "Distribution of trip type" , xlab = "Trip Type")
barplot(table(mydat$Payment_type),main = "Distribution of payment type" , xlab = "Payment type")
barplot(table(mydat$VendorID),main = "Distribution of vendor id" , xlab = "vendor Id")
barplot(table(mydat$Trip_distance),main = "Distribution of trip distance" , xlab = "Trip distance")

#Cleaning the data for the predictive model for tip as a percentage of the total fare
#Clean data for derived variable for tip as a percentage of the total fare.

mydat$Total_amount[mydat$Total_amount<2.5] <-2.5
mydat$Fare_amount[mydat$Fare_amount<0] <-0
mydat$improvement_surcharge[mydat$improvement_surcharge<0] <-0.30
mydat$Tip_amount[mydat$Tip_amount<0] <-0
mydat$Trip_type[mydat$Trip_type=="NaN"]<-1


mydat$RateCodeID[mydat$RateCodeID==99] <-2 
mydat$Ehail_fee<- NULL
mydat$Extra[mydat$Extra<0] <-0
Tip_Percentage<- (100*mydat$Tip_amount)/mydat$Total_amount
mydat$Tip_Percentage <- Tip_Percentage


Tip_Percentage
write.csv(mydat, file = "greenTaxicleanDataTip.csv", row.names = FALSE)
summary(mydat)
str(mydat)

cleanData<-mydat
# Change the format of datetime from string to POSIXct objects

cleanData$lpep_pickup_datetime <- as.POSIXct(cleanData$lpep_pickup_datetime,format='%Y-%m-%d %H:%M:%S')
cleanData$Lpep_dropoff_datetime <- as.POSIXct(cleanData$Lpep_dropoff_datetime,format='%Y-%m-%d %H:%M:%S')

cleanData$month <- month(cleanData$lpep_pickup_datetime)
cleanData$wday  <- wday(cleanData$lpep_pickup_datetime)
cleanData$hour  <- hour(cleanData$lpep_pickup_datetime)

cleanData$DropOffhour  <- hour(cleanData$Lpep_dropoff_datetime)
summary(cleanData)
cleanData$duration <- floor(as.double(cleanData$Lpep_dropoff_datetime-cleanData$lpep_pickup_datetime)/60.0)
#cleanData$TipPercentOnFareAmount  <- (cleanData$Tip_amount/cleanData$Fare_amount) * 100.0
cleanData$speed<- (60* cleanData$Trip_distance/cleanData$duration)
cleanData$speed[cleanData$speed=="NaN"]<-0
cleanData$speed[cleanData$speed=="Inf"]<-0



cleanData$BySpeed<-(cleanData$Payment_type==1 && cleanData$speed<80 && cleanData$Tip_Percentage<50) 
write.csv(cleanData, file = "greenTaxi1.csv", row.names = FALSE)

newLevel<-c(rep('0-6am',6),rep('6-9am',3),rep('9am-4pm',7),rep('4-7pm',3),rep('7-12pm',5))
levels(cleanData$hour)<-newLevel
levels
str(cleanData)
summary(cleanData)
#Applying Regression
library(lattice)
#splom(~cleanData[c(10,11,15)], groups = NULL, data = cleanData, axis.line.tck =0,axis.text.alpha=0)
VendorID<-as.factor(cleanData$VendorID)
Passenger_count<-as.factor(cleanData$Passenger_count)
Trip_distance<-as.factor(cleanData$Trip_distance)
Total_amount<-as.factor(cleanData$Total_amount)
Payment_type<-as.factor(cleanData$Payment_type)
Hour<-as.factor(cleanData$hour)
Week<-as.factor(cleanData$wday)
Month_day<-as.factor(cleanData$month)
Trip_duration<-as.factor(cleanData$duration)
Speed_mph<-as.factor(cleanData$speed)
Tolls_amount<-as.factor(cleanData$Tolls_amount)
Extra<-as.factor(cleanData$Extra)

#results <-lm(formula = Tip_Percentage ~  
 #               Total_amount + 
  #             Trip_duration + Speed_mph , data = trainn)




## 75% of the sample size
smp_size <- floor(0.02 * nrow(cleanData))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(cleanData)), size = smp_size)

trainn <- cleanData[train_ind, ]
#test <- cleanData[-train_ind, ]
summary(trainn)
str(trainn)














