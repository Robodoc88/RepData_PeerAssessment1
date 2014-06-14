# Reproducible Research: Peer Assessment 1

This is the Rmd file of the Peer Assessment No. 1 for Reproducible Research in Coursera.

The primary objectives of this assignment are:
 1. To read the `activity.csv` file from working directory
 2. To determine total steps taken each day, draw a histogram, and determine mean and median of daily steps.
 3. To draw a time series plot of average steps taken in 5 minute interval across all days, and to determine the 5 minute interval that contains maximum average steps taken.
 4. To determine the missing values and impute the missing values.  Compare the dateset that contains imputed values to the original dataset.
 5. To compare the activities between weekday and weekend.
 
The data was downloaded from the assignment website on June 5 2014 and unzipped using the following codes:

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
q <- p + ggtitle("Figure 1: Histogram of Daily Total Steps\nfrom 10-1-12 to 11-31-12") + 
    xlab("Daily Total Steps") + theme(plot.title = element_text(face = "bold")) + 
    geom_vline(xintercept = dailymean, color = "red", alpha = 0.4, size = 1.2) + 
    geom_vline(xintercept = dailymedian, color = "green", alpha = 0.4, size = 1.2) + 
    annotate("text", x = 13600, y = 10.3, label = "mean/median", color = "orange")
print(q)
```

![plot of chunk fig1](figure/fig1.png) 

In Figure 1, the red vertical line is mean, and the green vertical line is median.  Because these two values are very close, the lines are superimposed.

## What is the average daily activity pattern?

Average steps taken in each time interval is calculated.  The interval is not continuous.  They represent hour and minute of military time.  There is no interval between 60 and 95.  For example, interval 2005 represent 20:05.

```r
timed.activity <- aggregate(activity$steps, list(activity$interval), mean, na.rm = TRUE)
names(timed.activity) <- c("interval", "steps")
head(timed.activity)
```

```
##   interval   steps
## 1        0 1.71698
## 2        5 0.33962
## 3       10 0.13208
## 4       15 0.15094
## 5       20 0.07547
## 6       25 2.09434
```

```r
timed.activity$interval
```

```
##   [1]    0    5   10   15   20   25   30   35   40   45   50   55  100  105
##  [15]  110  115  120  125  130  135  140  145  150  155  200  205  210  215
##  [29]  220  225  230  235  240  245  250  255  300  305  310  315  320  325
##  [43]  330  335  340  345  350  355  400  405  410  415  420  425  430  435
##  [57]  440  445  450  455  500  505  510  515  520  525  530  535  540  545
##  [71]  550  555  600  605  610  615  620  625  630  635  640  645  650  655
##  [85]  700  705  710  715  720  725  730  735  740  745  750  755  800  805
##  [99]  810  815  820  825  830  835  840  845  850  855  900  905  910  915
## [113]  920  925  930  935  940  945  950  955 1000 1005 1010 1015 1020 1025
## [127] 1030 1035 1040 1045 1050 1055 1100 1105 1110 1115 1120 1125 1130 1135
## [141] 1140 1145 1150 1155 1200 1205 1210 1215 1220 1225 1230 1235 1240 1245
## [155] 1250 1255 1300 1305 1310 1315 1320 1325 1330 1335 1340 1345 1350 1355
## [169] 1400 1405 1410 1415 1420 1425 1430 1435 1440 1445 1450 1455 1500 1505
## [183] 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 1600 1605 1610 1615
## [197] 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710 1715 1720 1725
## [211] 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815 1820 1825 1830 1835
## [225] 1840 1845 1850 1855 1900 1905 1910 1915 1920 1925 1930 1935 1940 1945
## [239] 1950 1955 2000 2005 2010 2015 2020 2025 2030 2035 2040 2045 2050 2055
## [253] 2100 2105 2110 2115 2120 2125 2130 2135 2140 2145 2150 2155 2200 2205
## [267] 2210 2215 2220 2225 2230 2235 2240 2245 2250 2255 2300 2305 2310 2315
## [281] 2320 2325 2330 2335 2340 2345 2350 2355
```

The data is plotted with ggplot2.

```r
p2 <- ggplot(timed.activity, aes(interval, steps)) + geom_line(color = "blue")
q2 <- p2 + ggtitle("Figure 2: Time Series Plot of Average Steps in Each Interval") + 
    theme(plot.title = element_text(face = "bold")) + ylab("Average Steps") + 
    xlab("Interval")
print(q2)
```

![plot of chunk fig2](figure/fig2.png) 

Finding in which 5-minute interval lies maximal average steps:

```r
a <- timed.activity[timed.activity$steps == max(timed.activity$steps), ]
print(paste("The maximaum of", round(a$steps, digits = 1), "steps is located in the interval", 
    a$interval))
```

```
## [1] "The maximaum of 206.2 steps is located in the interval 835"
```

The interval is **835** (8:35).

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?











