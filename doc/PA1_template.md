#read the data
activity<-read.csv("C:/Users/Arnela/Desktop/coursera/activity.csv")
summary(activity)

#Some information about the variables
str(activity)
names(activity)
head(activity)

#calculate the total number of steps taken per day
StepsPerDay<-aggregate(steps~date,data = activity,sum, na.rm=TRUE)
colnames(StepsPerDay)<-c("Date","Steps")
StepsPerDay

#histogram 
hist(StepsPerDay$Steps)

#mean
mean(StepsPerDay$Steps,na.rm = TRUE)
#median
median(StepsPerDay$Steps,na.rm = TRUE)

#create table with steps per time
StepsPerInterval<-aggregate(steps~interval,data = activity,mean,na.rm=TRUE)
#variable time
StepsPerInterval$time<-StepsPerInterval$interval/100
plot(steps~interval,data = StepsPerInterval)

#The 5-minute interval accross all day containig the maximum nuber od steps is stored in variable MaxNbSteps
MaxNbSteps<-StepsPerInterval[which.max(StepsPerInterval$steps),]$interval
MaxNbSteps

#total number of missing values
totalVM<-sum(is.na(activity$steps))
totalVM

#replacing missing value
getMeanStepsPerInterval<-function(interval){
  StepsPerInterval[StepsPerInterval$interval==interval,]$steps
}

#new dataset is activityFull
activityFull<-activity
for(i in 1:nrow(activityFull)){
  if(is.na(activityFull[i,]$steps)){
    activityFull[i,]$steps<-getMeanStepsPerInterval(activityFull[i,]$interval)}
}

#histogram of the total number of steps taken each day with missing data filled in
totalStepsPD<-aggregate(steps~date,data=activityFull,sum)
hist(totalStepsPD$steps)

#mean
mean(totalStepsPD$steps)
#median
median(totalStepsPD$steps)

#new factor variable with weekdays and weekend day
activityFull$date<-as.Date(activityFull$date, format = "%Y-%m-%d")
activityFull$day<-weekdays(activityFull$date)
for (i in 1:nrow(activityFull)) {
  if (activityFull[i,]$day %in% c("Saturday","Sunday")){
    activityFull[i,]$day<-"weekday"
  }
}

#plot
stepsByday<-aggregate(activityFull$steps~activityFull$interval+activityFull$day, activityFull,mean)
names(stepsByday)<-c("interval","day","steps")
library(lattice)
xyplot(steps~interval|day, stepsByday, type="1",layout=c(1,2),xlab="Interval",ylab = "Number of steps")