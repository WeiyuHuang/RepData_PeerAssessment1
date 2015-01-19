---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
# Reproducible Research: Peer Assessment 1

First set defaults

```r
opts_chunk$set(echo = TRUE, fig.heights = 5, cache = FALSE)
```

## Loading and preprocessing the data

```r
setwd("/Users/weiyuhuang/Documents/GitHub/RepData_PeerAssessment1/")
data = read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

* Make a histogram of the total number of steps taken each day

```r
# Remove Nas and subset only STEPS and DATES
data.stepPerDay <- data[!is.na(data[, 1]), 1:2]
# Plot histogram
data.stepPerDay.results <- aggregate(steps ~ date, data.stepPerDay, sum)
hist(data.stepPerDay.results[, 2], breaks = 10, main = "Total number of steps taken each day", xlab = "Total number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

* Calculate and report the **mean** and **median** total number of steps taken per day

```r
# Calculate and report mean and median values
data.stepPerDay.mean <- mean(data.stepPerDay.results[, 2])
data.stepPerDay.median <- median(data.stepPerDay.results[ ,2])
```
The mean for total number of steps taken each day is 1.0766 &times; 10<sup>4</sup> and median 10765.


## What is the average daily activity pattern?

* Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
# Make the time series plot
data.aveDailyAct <- data[!is.na(data[, 1]), ]
temp <- aggregate(data.aveDailyAct$steps, list(interval = data.aveDailyAct$interval), mean)
plot(temp$interval, temp$x, main = "Average steps of a given interval", type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# Find the interval yielding maximum number of steps
data.aveDailyAct.maxInteral <- temp$interval[which.max(temp$x)]
```

The 5-minute interval, on average across all the days in the dataset, contins the maximum number of steps is 835.

## Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
data.totalNa <- sum(rowSums(is.na(data)))
```
The total number of missing values in the dataset is 2304.

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Then create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data.impute <- data
temp <- aggregate(steps ~ date, data.stepPerDay, mean)
idx <- which(is.na(data.impute$steps)) # idx for NAs
data.impute$steps[idx] <- temp$steps[data.impute$date[idx]]
# Still, data.stepPerDay is undefined for some days, lets make them the average over all days
data.impute$steps[is.na(data.impute$steps)] <- mean(data.stepPerDay$steps)
```

* Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

* Make a histogram of the total number of steps taken each day

```r
# Plot histogram
data.impute.results <- aggregate(steps ~ date, data.impute, sum)
hist(data.impute.results$steps, breaks = 10, main = "Total number of steps taken each day", xlab = "total number of steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
# Calculate and report mean and median values
data.impute.mean <- mean(data.impute.results$steps)
data.impute.median <- median(data.impute.results$steps)
```
The mean for total number of steps taken each day is 1.0453 &times; 10<sup>4</sup> and median 1.06 &times; 10<sup>4</sup>.

By imputing missing data, the estimates of the total daily number of steps do not change a lot whereas in the meantime we remove all NAs in a reasonable way.

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
temp <- weekdays(as.Date(data$date)) %in% c('Saturday', 'Sunday')
data$day <- factor(temp, labels = c('weekday', 'weekend'))
```

* Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:


```r
data.day <- data[!is.na(data[, 1]), ]
temp <- aggregate(steps ~ interval + day, data.day, mean)
library("lattice")
xyplot(steps ~ interval | day, data = temp, type = "l", layout = c(1,2), ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
