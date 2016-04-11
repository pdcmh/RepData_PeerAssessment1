# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

- Load required liabraries for the assignment

```r
library(knitr)
opts_chunk$set(cache=TRUE)
```

- Load .csv Data.  

```r
df.activity <- read.csv('activity.csv', header = TRUE, sep = ",")
```
## What is mean total number of steps taken per day?

- Calculate the total number of steps taken per day

```r
agg.stepsperday <- aggregate(steps ~ date, df.activity, sum)
head(agg.stepsperday)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
- Make a histogram of the total number of steps taken each day                     


```r
hist(agg.stepsperday$steps, xlab="Total steps per day", ylab="Days",main="Histogram of the total number of steps taken each day")
```

![](PA1_Assignment_files/figure-html/unnamed-chunk-3-1.png) 
             
- Calculate and report the mean and median of the total number of steps taken per day

```r
mean(agg.stepsperday$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(agg.stepsperday$steps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avg.steps5min <- aggregate(steps ~ interval, df.activity, mean)
plot(avg.steps5min, type="l",xlab="interval [in 5min]", ylab="Average daily activity pattern of steps",  main="average number of steps")
```

![](PA1_Assignment_files/figure-html/unnamed-chunk-5-1.png) 

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avg.steps5min$interval[which.max(avg.steps5min$steps)]
```

```
## [1] 835
```

## Imputing missing values

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(df.activity))
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# Filling the daily NA values with 5-min interval mean of the same day 
df.activityAll = merge(df.activity, avg.steps5min, by="interval")
df.activityAll$steps.x[is.na(df.activityAll$steps.x)] = df.activityAll$steps.y[is.na(df.activityAll$steps.x)]

# Checking that NO missing values in the resulting dataset
sum(is.na(df.activityAll))
```

```
## [1] 0
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
df.activityAll <- aggregate(steps.x ~ interval,df.activityAll,sum)
hist(df.activityAll$steps.x, xlab="Total steps taken by day", ylab="Days",main="Histogram of the total number of steps taken each day")
```

![](PA1_Assignment_files/figure-html/unnamed-chunk-9-1.png) 

- Mean and Median after filling missing values


```r
mean(df.activityAll$steps)
```

```
## [1] 2280.339
```

```r
median(df.activityAll$steps)
```

```
## [1] 2080.906
```

- **Imapct**: Steps per day chaged from more that 10,000 to less than 2,500 per day.  

  
## Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
fun.dayofweek <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } 
    else {
        "weekday"
    }
}
df.activity$fun.dayofweek <- as.factor(sapply(df.activity$date, fun.dayofweek))
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
par(mfrow=c(2,1))
for (day in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, df.activity,subset=df.activity$fun.dayofweek==day, mean)
    plot(steps.type, type="l", main=day)
}
```

![](PA1_Assignment_files/figure-html/unnamed-chunk-12-1.png) 
