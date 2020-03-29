---
title: "Reproducible Research: Peer Assessment 1"
author: "Hestia Zhang"
output: 
  html_document:
    keep_md: true
---



## Loading libraries


```r
library(tidyverse)
```

## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the mean and median total number of steps taken per day


```r
steps_day <- activity %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))
    
ggplot(steps_day, aes(x = total_steps)) +
    geom_histogram(bins = 30)
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_Hestia_files/figure-html/histogram-1.png)<!-- -->

```r
mean(steps_day$total_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_day$total_steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
average <- activity %>%
    group_by(interval) %>%
    summarise(steps = mean(steps, na.rm = TRUE))

ggplot(average, aes(x = interval, y = steps)) +
    geom_line()
```

![](PA1_Hestia_files/figure-html/interval-1.png)<!-- -->

```r
filter(average, steps == max(steps))
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
count(activity, is.na(steps))
```

```
## # A tibble: 2 x 2
##   `is.na(steps)`     n
##   <lgl>          <int>
## 1 FALSE          15264
## 2 TRUE            2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. Here we replace the missing values with the mean for each 5-minute interval.


```r
average_interval <- activity %>%
    group_by(interval) %>%
    summarise(average_steps = mean(steps, na.rm = TRUE))

activity1 <- merge(activity, average_interval, by = "interval", all = TRUE)

activity1$steps <- ifelse(is.na(activity1$steps), activity1$average_steps, activity1$steps)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
write.csv(activity1, "activity_filled.csv")
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
activity_day <- activity1 %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))

ggplot(activity_day, aes(x = total_steps)) +
    geom_histogram(bins = 30)
```

![](PA1_Hestia_files/figure-html/histogram2-1.png)<!-- -->

```r
mean(activity_day$total_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(activity_day$total_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
weekdays <- activity %>%
    mutate(date = as.Date(date)) %>%
    mutate(day = weekdays(date)) %>%
    mutate(weekdays = ifelse(day %in% c("星期六","星期日"), "weekend", "weekday")) %>%
    mutate(weekdays = as.factor(weekdays))

steps_week <- weekdays %>%
    group_by(weekdays, interval) %>%
    summarise(average_steps = mean(steps, na.rm = TRUE))

ggplot(steps_week, aes(x = interval, y = average_steps, colour = weekdays)) + 
    geom_line() +
    facet_wrap(~weekdays, ncol = 1, nrow = 2)
```

![](PA1_Hestia_files/figure-html/weekdays-1.png)<!-- -->


