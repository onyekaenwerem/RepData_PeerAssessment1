# Reproducible Research: Peer Assessment 1


```r
packages <- c("dplyr", "ggplot2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
```

```
##   dplyr ggplot2 
##    TRUE    TRUE
```

## Loading and preprocessing the data
1. Load the data

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activityDT <- read.csv('activity.csv')
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
StepsPerDay <- tapply(activityDT$steps, activityDT$date, sum, na.rm=TRUE)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
qplot(StepsPerDay, binwidth=1000, xlab="Total steps per day", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
StepsPerDayMean <- mean(StepsPerDay)
StepsPerDayMedian <- median(StepsPerDay)
```
- Mean of total number of steps per day: 9354.2295082
- Median: of total number of steps per day: 10395


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
AvgStepPerInterval<- activityDT %>%
        group_by(interval) %>% 
        summarise(AvgSteps = mean(steps, na.rm=TRUE))
```


```r
ggplot(data=AvgStepPerInterval, aes(x=interval, y=AvgSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
MaxNumofSteps <- which.max(AvgStepPerInterval$AvgSteps)
timeMaxNumofSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", AvgStepPerInterval[MaxNumofSteps,"interval"])
```
Most Steps at: 8:35

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NumMissingValue <- sum(is.na(activityDT$steps))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
Filling in missing value with average steps per 5 minute intervla

```r
DTFilled <- activityDT
UniqueInterval<-unique(DTFilled$interval)
        for (i in 1:length(UniqueInterval)){
        DTFilled[(DTFilled$interval == UniqueInterval[i]) & is.na(DTFilled$steps),][,"steps"] <- AvgStepPerInterval$AvgSteps[i]
        }
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
StepsPerDayFilled <- tapply(DTFilled$steps, DTFilled$date, sum)
qplot(StepsPerDayFilled, binwidth=1000, xlab="Total steps per day", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
StepsPerDayFilledMean <- mean(StepsPerDayFilled)
StepsPerDayFilledMedian <- median(StepsPerDayFilled)
```
- Mean of total number of steps per day with missing values filled: 1.0766189\times 10^{4}
- Median: of total number of steps per day with missing values filled: 1.0766189\times 10^{4}
```

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
DTFilled$dateWeek <- ifelse(weekdays(as.POSIXlt(DTFilled$date)) %in% c("Saturday", "Sunday"), "weekend", "weekday")
DTFilled$dateWeek <- as.factor(DTFilled$dateWeek)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
AvgStepPerIntervalFilled <- DTFilled %>% 
group_by(interval, dateWeek) %>%
summarise(AvgSteps = mean(steps, na.rm=TRUE))

ggplot(AvgStepPerIntervalFilled, aes(interval, AvgSteps)) + 
    geom_line() + 
    facet_grid(dateWeek ~ .) +
    xlab("5-minute interval") + 
    ylab("average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

