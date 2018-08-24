---
title: "PA1_template.md"
author: "Jacob Asariah"
date: "August 23, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, results="hide")
```

# Reproducible Research Course Project 1

### Load the data and Process/transform the data (if necessary) into a format suitable for your analysis.

```{r}
activitydf <- read.csv(file="activity.csv", header=TRUE, sep=",")

summary(activitydf)
```

## What is mean total number of steps taken per day?


```{r}
DailyStepsCount <- aggregate(steps ~ date, subset(activitydf,!is.na(activitydf$steps)), sum)

hist(DailyStepsCount$steps,col="green",xlab="Daily Total Steps",ylab="Days",main="Daily Steps Count")


```

### Calculate and report the mean and median of the total number of steps taken per day


```{r}
mean(DailyStepsCount$steps)

median(DailyStepsCount$steps)
```

## What is the average daily activity pattern?

```{r}
IntervalCount <-aggregate(steps~interval, data=activitydf, mean, na.rm=TRUE)
plot(steps~interval, data=IntervalCount, type="l")

```

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```{r}
IntervalCount[which.max(IntervalCount$steps),]$interval


```


## Imputing missing values

### Calculate and report the total number of missing values in the dataset.

```{r}
sum(is.na(activitydf$steps))
```


### Devise a strategy for filling in all of the missing values in the dataset. 
### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
activitydfNA <- activitydf
activitydfNA[["steps"]][is.na(activitydfNA[["steps"]])] <- 0
DailyStepsCount <- aggregate(steps ~ date, subset(activitydfNA,!is.na(activitydf$steps)), sum)

```


### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```{r}
hist(DailyStepsCount$steps,col="green",xlab="Daily Total Steps - NA Filled",ylab="Days",main="Daily Steps Count - NA Filled")

```

### Calculate and report the mean and median total number of steps taken per day.



```{r}
mean(DailyStepsCount$steps)

median(DailyStepsCount$steps)
```



## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```{r}
activitydfNA$date <- as.Date(strptime(activitydfNA$date, format="%Y-%m-%d"))

weekdaysdf <- transform(activitydfNA, wday = ifelse(weekdays(activitydfNA$date) =="Saturday" | weekdays(activitydfNA$date) =="Sunday" ,"weekend","weekday"))


```

### Make a panel plot containing a time series plot type="l" of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 



```{r}
library(lattice)

xyplot(steps ~ interval | wday, weekdaysdf, type = "l", layout = c(1, 2),xlab = "Interval", ylab = "Number of steps")

```



