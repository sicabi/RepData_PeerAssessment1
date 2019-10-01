## 1. Loading and preprocessing the data
unzip("activity.zip", list = FALSE)
activity.data <- read.csv(file = "activity.csv", stringsAsFactors = FALSE,
                           header = TRUE, fileEncoding = "latin1")
str(activity.data)
activity.data$date <-  as.Date(activity.data$date, tz = Sys.timezone())
str(activity.data)
## 2. What is the mean total number of steps taken per day?
total.steps <- sum(activity.data$steps, na.rm = TRUE)
print(total.steps)
total.days <- length(unique(activity.data$date))
print(total.days)
mean.steps <- total.steps/total.days
print(mean.steps)
### 2.1 Calculate the total number of steps taken per day
steps.per.day <- aggregate(formula = steps ~ date, data = activity.data, FUN = sum)
print(steps.per.day)
### 2.2 Make a histogram of the total number of steps taken each day
hist(steps.per.day$steps, breaks = "Sturges", main = "Total number of steps taken each day", xlab = "Steps", ylab = "Days")
median(steps.per.day$steps)
mean(steps.per.day$steps)
intervals <- aggregate(steps ~ interval, data = activity.data, sum)
str(intervals)
names(intervals)[2] <- "total.steps.interval"
names(intervals)
intervals$mean.steps.interval <- intervals$total.steps.interval/length(unique(activity.data$date))
str(intervals)
### 3.1 Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
with(intervals, plot(x = interval, y = mean.steps.interval, type = "l", 
                     main = "Average number of steps taken by interval per day",
                     xlab = "5-minute interval",
                     ylab = "Average number of steps",
                     xaxt  = "n"))
axis(side = 1, at = seq(from = 0, to = 2300, by = 100), 
     labels = seq(from = 0, to = 2300, by = 100))
### 3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
intervals[which.max(intervals$mean.steps.interval), ]
## 4. Imputing missing values
### 4.1 Calculate and report the total number of missing values in the dataset
colSums((is.na(activity.data)))
activity.data$missing.steps <- ifelse(is.na(activity.data$steps), 1, 0)
aggregate(formula = missing.steps ~ date, data = activity.data, FUN = sum)
aggregate(formula = missing.steps ~ interval, data = activity.data, FUN = sum)
### 4.2 Fill the missing values in the dataset - mean for the 5-minute interval
imputed.steps <- ifelse(is.na(activity.data$steps) & 
                                activity.data$interval == intervals$interval, 
                        intervals$mean.steps.interval, activity.data$steps)
### 4.3 Create a new dataset that is equal to the original dataset but with the missing data filled in
new.activity.data <- activity.data
new.activity.data$steps <- imputed.steps
str(activity.data)
str(new.activity.data)
### 4.4 Make a histogram of the total number of steps taken each day
new.steps.per.day <- aggregate(formula = steps ~ date, data = new.activity.data, FUN = sum)
hist(new.steps.per.day$steps, breaks = "Sturges", 
     main = "Total number of steps taken each day - 5-minute interval imputation",
     xlab = "Steps", ylab = "Days")
### 4.5  Calculate and report the mean and median total number of steps taken per day
mean(steps.per.day$steps, na.rm = TRUE)
median(steps.per.day$steps, na.rm = TRUE)
mean(new.steps.per.day$steps)
median(new.steps.per.day$steps)
## 5. Are there differences in activity patterns between weekdays and weekends?
new.activity.data$weekday <- weekdays(new.activity.data$date, abbreviate = TRUE)
new.activity.data$weekend <- ifelse(new.activity.data$weekday == "Sat" | new.activity.data$weekday == "Sun", "Weekend", "Weekdays")
new.activity.data$weekend <- as.factor(new.activity.data$weekend)
intervals.weekends <- aggregate(formula = steps ~ interval + weekend, data = new.activity.data, FUN = mean)
str(intervals.weekends)
library("lattice")
xyplot(steps ~ interval | as.factor(weekend), data = intervals.weekends, 
       type = "l", layout = c(1,2),
       main = "Average number of steps done by interval per day type", 
       ylab = "Steps", xlab = "Time (5-minute interval)",
       panel = function(x, y, ...){
               panel.xyplot(x, y, ...)
               panel.abline(h = mean(y), lty = 2)
       })
mean(intervals.weekends[intervals.weekends$weekend == "Weekdays","steps"])
mean(intervals.weekends[intervals.weekends$weekend == "Weekend","steps"])