#
# Module  : Reproducible Research
# Project : Week 1 Project - Activity monitoring data using Dataset [52K]
# FileName: Final_M6_P1.r
#

#
# Objectives:
# 1. Code for reading in the dataset and/or processing the data
# 2. Histogram of the total number of steps taken each day
# 3. Mean and median number of steps taken each day
# 4. Time series plot of the average number of steps taken
# 5. The 5-minute interval that, on average, contains the maximum number of steps
# 6. Code to describe and show a strategy for imputing missing data
# 7. Histogram of the total number of steps taken each day after missing values are imputed
# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
#

library(httr)   # Used to get data from Source.

#
# get Data 
#

# Initialize Variables.

downloadfile <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipfile      <- "./data/repdata_data_activity.zip"
zipdir       <- "./data"
datafile     <- "./data/activity.csv"

# Create Download (data) directory if not exists in current wd.

if (!file.exists(zipdir))  # Check if data folder exists in getwd()
{ 
  dir.create(zipdir)       # Create it if not available.
}    

# 
# Objective:
# 1. Code for reading in the dataset and/or processing the data
#
# Read Data File as given in Project Paper.
# Mandatory data folder exists in getwd() folder.
# 

if (!file.exists(zipfile)) {
  download.file(downloadfile, destfile=zipfile, cacheOK=TRUE, mode="wb", method="auto")
  if (file.exists(zipfile)) {
    #     filelist<-unzip(zipfile, overwrite=TRUE, list=TRUE, exdir=zipdir)
    unzip(zipfile, overwrite=TRUE, exdir=zipdir)
  }
} 

# Read Activity monitoring data ('activitydata') data set
activitydata <- read.csv(datafile, 
                         colClasses=c("integer", "Date", "integer"), 
                         header=TRUE,
                         stringsAsFactors = FALSE)

#
# Data Cleaning
#

# 1a. convert Date Column to Date class
# 1b. Add DateTime Column with DAte & Time in datetime format
# Convert Interval Data into Time Data using Date & Interval Field.
activitydata$date    <- as.POSIXct(strptime(activitydata$date, "%Y-%m-%d"),tz="")
# first convert integer time to character and pad with leading zeros... and then to date type
activitydata$time    <- sprintf("%04d", activitydata$interval)                 
activitydata$time    <- as.POSIXct(activitydata$time, "%H%M",tz="")
# End New Added ---

# Summarize Data (View Data)
# summary(activitydata)
# head(activitydata)
# str(activitydata)

#
# Objective: What is mean total number of steps taken per day?
#
# 1. Make a histogram of the total number of steps taken each day
# 2. Calculate and report the mean and median total number of steps taken per day
#

#
# 1. Make a histogram of the total number of steps taken each day
# 
# First get Total steps walked on a day.
# Then get the average of Steps for entire data.
#
steps_by_date <- aggregate(list(total_steps = activitydata$steps),
                           by=list(date = activitydata$date),
                           FUN=sum,
                           na.rm=TRUE)
steps_by_date_mean   <- mean(steps_by_date$total_steps)
steps_by_date_median <- median(steps_by_date$total_steps)

#
# Total Steps taken each Day
#
total_steps_by_date <- aggregate(list(total_steps = steps_by_date$total_steps),
                                 by=list(date = steps_by_date$date),
                                 FUN=sum,
                                 na.rm=TRUE)

#
# Draw Histogram
# Parameter: breaks to be automated later.
#

dev.new()

hist(total_steps_by_date$total_steps, 
     breaks = 20, 
     col = "red", 
     plot=TRUE,
     labels=TRUE,
     xlab = "Number of Steps", 
     main= "Total number of steps taken each day")

# Show Mean & Median
abline(v=steps_by_date_mean, col="blue", lwd=3)
abline(v=steps_by_date_median, col="green", lwd=3)

# Show Legend for Mean & Median
legend(x="topright", 
       legend=c(paste("Mean: ",steps_by_date_mean),
                paste("Median: ",steps_by_date_median)), 
       col=c("blue","green"), bty="n", lwd=3)

dev.copy(png, file="plot1.png", width=480, height=480)
dev.off()

#
# Objective 2. What is the average daily activity pattern?
#
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#    and the average number of steps taken, averaged across all days (y-axis)
# 2. Which 5-minute interval, on average across all the days in the dataset, 
#    contains the maximum number of steps?
# 

avg_steps_by_timeint <- aggregate(list(avg_steps = activitydata$steps),
                                  by=list(interval = activitydata$interval),
                                  FUN=mean,
                                  na.rm=TRUE)

avgsteps =avg_steps_by_timeint[which.max(avg_steps_by_timeint$avg_steps),]$avg_steps
avgsteptm=avg_steps_by_timeint[which.max(avg_steps_by_timeint$avg_steps),]$interval

dev.new()

plot(avg_steps ~ interval,
     data=avg_steps_by_timeint,
     xlab="Time interval",
     ylab="Mean steps",
     main="Mean Steps By Time Interval",
     type="l",
     col="blue",
     lwd=2)
abline(v=avgsteptm, col="red", lwd=3)
legend(x="topright", 
       legend=c(paste("Max Steps : ", avgsteps , " @ ", avgsteptm)),
       col=c("blue"), bty="n", lwd=3)

dev.copy(png, file="plot2.png", width=480, height=480)
dev.off()

#
# Objective : Imputing missing values
#
# Note that there are a number of days/intervals where there are missing values 
# (coded as NA). The presence of missing days may introduce bias into some 
# calculations or summaries of the data.
# 1. Calculate and report the total number of missing values in the dataset 
#    (i.e. the total number of rows with NAs)
# 2. Devise a strategy for filling in all of the missing values in the dataset. 
#    The strategy does not need to be sophisticated. For example, you could use 
#    the mean/median for that day, or the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the 
#    missing data filled in.
# 4. Make a histogram of the total number of steps taken each day and Calculate 
#    and report the mean and median total number of steps taken per day. 
#    Do these values differ from the estimates from the first part of the assignment? 
#    What is the impact of imputing missing data on the estimates of the total daily 
#    number of steps?
#

# Report number of Missing Data (NA)
sum(is.na(activitydata$steps))   # No of NA Rows.

#
# Clean NA Data with mean for the mean for same time Interval 
#

#
# "join" the two data frames using merge()
# Creating a new dataset
#
activity_imputed <- merge(activitydata,
                          avg_steps_by_timeint,
                          by_x="interval")

# correct the NA steps with average steps facor the interval
activity_imputed <- within(activity_imputed,
                           steps <- ifelse(is.na(activity_imputed$steps),
                                           activity_imputed$avg_steps,
                                           activity_imputed$steps))

# Check Observations
# str(activitydata); View(activitydata)        # get num of Observations in original Dataset
# str(activity_imputed);View(activity_imputed) # get num of Observations after imputation

#
# Start Aggregation by Date
#
ampsteps_by_date <- aggregate(list(total_steps = activity_imputed$steps),
                           by=list(date = activity_imputed$date),
                           FUN=sum,
                           na.rm=TRUE)

ampsteps_by_date_mean   <- mean(ampsteps_by_date$total_steps)
ampsteps_by_date_median <- median(ampsteps_by_date$total_steps)

# Testing
# 
# print(paste("Mean steps taken each day   (Amp) : ",ampsteps_by_date_mean))
# print(paste("Median steps taken each day (Amp) : ",ampsteps_by_date_median))
#

# 
# Plot Histogram
#

dev.new()

hist(ampsteps_by_date$total_steps, 
     breaks=25,
     xlab="Steps/Day", 
     labels=TRUE,
     main="Histogram of total steps per day")
abline(v=ampsteps_by_date_mean, col="blue", lwd=3)
legend(x="topright", 
       legend=c(paste("Mean   (Amp): ",ampsteps_by_date_mean),
                paste("Median (Amp): ",ampsteps_by_date_median)), 
       col=c("blue","red"), bty="n", lwd=3)

dev.copy(png, file="plot3.png", width=480, height=480)
dev.off()

# 
# Objective : Are there differences in activity patterns between weekdays and weekends?
#
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in 
# missing values for this part.
#
# 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#    indicating whether a given date is a weekday or weekend day.
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#    and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
#    See the README file in the GitHub repository to see an example of what this plot should look like 
#    using simulated data.
#

library(plyr)
library(lattice)

activity_imputed$Weekend <- weekdays(activity_imputed$date) == "Saturday" | 
                            weekdays(activity_imputed$date) == "Sunday"

activity_imputed$Weekend <- factor(activity_imputed$Weekend, 
                                   levels = c(F, T), 
                                   labels = c("Weekday", "Weekend"))
activity_imputed <- ddply(activity_imputed, 
                          .(interval, Weekend), 
                          summarize, 
                          steps = mean(steps, na.rm = T))

dev.new()
xyplot(steps ~ interval | Weekend, activity_imputed, 
       type = "l", 
       layout = c(1, 2), 
       ylab = "Number of Steps", 
       xlab = "Interval", 
       main = "Time Series for Weekend and Weekday Activity Patterns")

dev.copy(png, file="plot4.png", width=480, height=480)
dev.off()