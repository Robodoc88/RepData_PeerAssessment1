# Reproducible Research: Peer Assessment 1

This is the Rmd file of the Peer Assessment No. 1 for Reproducible Research in Coursera.

The primary objectives of this assignment are:
 1. To read the `activity.csv` file from working directory
 2. To determine total steps taken each day, draw a histogram, and determine mean and median of daily steps.
 3. To draw a time series plot of average steps taken in 5 minute interval across all days, and to determine the 5 minute interval that contains maximum average steps taken.
 4. To determine the missing values and impute the missing values.  Compare the dateset that contains imputed values to the original dataset.
 5. To compare the activities between weekday and weekend.
 
The data was downloaded from the assignment website on June 5 2014 and unzip using the following codes:

`fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"`

`download.file(fileURL, dest="activity.zip", method = "auto")`

`unzip("activity.zip")`

I avoid running the code repeatedly to prevent overload of the website.
 
## Loading and preprocessing the data

The following codes read the `activity.csv` file and add `wd` column for day of the week

```r
library(lubridate)
activity <- read.csv("activity.csv", header = T)
activity$wd <- wday(activity$date, label = T)
summary(activity)
```

```
##      steps               date          interval        wd      
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0   Sun  :2304  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589   Mon  :2592  
##  Median :  0.0   2012-10-03:  288   Median :1178   Tues :2592  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178   Wed  :2592  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766   Thurs:2592  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355   Fri  :2592  
##  NA's   :2304    (Other)   :15840                  Sat  :2304
```

## What is mean total number of steps taken per day?

Total activity for each day, mean and median of total activity were calculated.

```r
total.activity <- aggregate(activity$steps, list(activity$date), sum)
names(total.activity) <- c("date", "steps")
head(total.activity)
```

```
##         date steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
dailymean <- round(mean(total.activity$steps, na.rm = T), digits = 2)
dailymedian <- median(total.activity$steps, na.rm = T)
print(paste("Mean of the total daily activity:", dailymean, "steps."))
```

```
## [1] "Mean of the total daily activity: 10766.19 steps."
```

```r
print(paste("Median of the total daily activity:", dailymedian, "steps."))
```

```
## [1] "Median of the total daily activity: 10765 steps."
```

Draw histogram with ggplot2

```r
library(ggplot2)
p <- ggplot(total.activity, aes(steps)) + geom_histogram(fill = "blue", color = "yellow", 
    binwidth = 1000)
q <- p + ggtitle("Fig 1: Histogram of Daily Total Steps\nfrom 10-1-12 to 11-31-12") + 
    xlab("Daily Total Steps") + theme(plot.title = element_text(face = "bold")) + 
    geom_vline(xintercept = dailymean, color = "red", alpha = 0.4, size = 1.2) + 
    geom_vline(xintercept = dailymedian, color = "green", alpha = 0.4, size = 1.2) + 
    annotate("text", x = 13600, y = 10.3, label = "mean/median", color = "green")
print(q)
```

![plot of chunk fig1](figure/fig1.png) 

In Fig 1, the red vertical line is mean, and the green vertical line is median.  Because these two values are very close, the lines are superimposed.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?











