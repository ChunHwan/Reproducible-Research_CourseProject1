######################################################################
####           1. Loading and preprocessing the data              ####
######################################################################

  setwd("./")
  file_path <- "activity.csv"
  if(!file.exists(file_path)){ 
      unzip(zipfile="repdata_data_activity.zip")     
  }

  activity_Data <- read.csv("activity.csv", colClasses = c("numeric", "character", "integer"))
  # str(activity_Data)

  ## Convert Date type for activity_Data$date
  activity_Data$date <- as.POSIXct(activity_Data$date, format="%Y-%m-%d")


######################################################################
####     2. What is mean total number of steps taken per day?     ####
######################################################################

  sum_by_date <- aggregate(activity_Data$steps, by=list(activity_Data$date), FUN=sum, na.rm=TRUE)
  names(sum_by_date) = c("date", "steps")
  # str(sum_by_date)

  ## Make plot for Total number of steps per day
  hist(sum_by_date$steps,  breaks=seq(from=0, to=25000, by=2500), 
       main = "Total number of steps per day", xlab = "steps per day", ylab = "Frequency", col = "red") 

######################################################################
####     3. What is the average daily activity pattern?           ####
######################################################################

  mean_by_Interval <- aggregate(activity_Data$steps, by=list(activity_Data$interval), FUN=mean, na.rm=TRUE)
  names(mean_by_Interval) = c("Interval", "mean")
  # str(mean_by_Interval)

  ## Make plot for Average daily activity pattern
  plot(mean_by_Interval$Interval,mean_by_Interval$mean, type="l", 
       xlab="Interval", ylab="mean by steps", main="Average daily activity pattern")  

  ## identify maximun mean value for Interval
  mean_by_Interval[which.max(mean_by_Interval$mean),]

######################################################################
####               4. Imputing missing values                     ####
######################################################################

  ## Calculate and report the total number of missing values in the dataset 
  sum_num_NA <- sum(is.na(activity_Data$steps))

  ## replicate mean value instead of NA by func & 
  ## Create a new dataset that is equal to the original dataset but with the missing data filled in
  func_rep_mean_for_NA <- function(steps, mean) { 
  	return_para <- NA 
  	if (!is.na(steps)) 
		return_para <- steps 
	else 
		return_para <- mean
	return(return_para) 
  } 

  mean_steps <- mean(activity_Data$steps, na.rm=TRUE)
  New_activity_Data <- activity_Data 
  New_activity_Data$steps <- mapply(func_rep_mean_for_NA, New_activity_Data$steps, mean_steps) 

  ## Make histagram for Total number of steps per day (replaced NA by Mean Value)
  sum_by_date_not_NA <- aggregate(New_activity_Data$steps, by=list(New_activity_Data$date), FUN=sum, na.rm=TRUE)
  names(sum_by_date_not_NA) = c("date", "steps")

  hist(sum_by_date_not_NA$steps,  breaks=seq(from=0, to=25000, by=2500), 
       main = "Total number of steps per day (replaced NA by Mean Value)", 
       xlab = "steps per day", ylab = "Frequency", col = "red") 

##############################################################################################
####    5. Are there differences in activity patterns between weekdays and weekends?      ####
##############################################################################################

  ## Add Weekday & daytype to columns
  Weektype_activity_Data <- data.frame(steps=activity_Data$steps, date=activity_Data$date, interval=activity_Data$interval, 
                                       weekday=weekdays(activity_Data$date),
                                       daytype=ifelse(Weektype_activity_Data$weekday == "saturday" | Weektype_activity_Data$weekday == "sunday" | 
                                                      Weektype_activity_Data$weekday == "토요일" | Weektype_activity_Data$weekday == "일요일",
                                                      "weekend", "weekday"))

  ## Make divided histagrams for Weekend & Weekday
  Weektype_mean_data <- aggregate(Weektype_activity_Data$steps, by=list(Weektype_activity_Data$daytype, 
                                  Weektype_activity_Data$weekday, Weektype_activity_Data$interval), FUN=mean, na.rm=TRUE)
  names(Weektype_mean_data) <- c("daytype", "weekday", "interval", "mean")

  library(lattice)
  xyplot(mean ~ interval | daytype, Weektype_mean_data, type="l", lwd=1, xlab="Interval", ylab="steps", layout=c(1,2))
