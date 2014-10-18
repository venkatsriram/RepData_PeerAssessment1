# Reproducible Research: Peer Assessment 1


```r
library(knitr)
options(digits=7)
opts_chunk$set(warning=FALSE, fig.height=4, fig.width=8, fig.path="figure/")
```

## Loading and preprocessing the data
##### 1) Unzip the zipped data file, read the datafile, preprocess date column that is in string format to date format


```r
zipfile <- "activity.zip"
if (file.exists(zipfile)) {
        zipname <- unzip(zipfile, list=TRUE)
        unzip(zipfile)
        datafile <- zipname$Name
}

activity <- read.csv(datafile, header=TRUE, sep=",", stringsAsFactors=FALSE)
```

##### 2) Preprocess the read data which has date column in string format to date format


```r
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
```
## What is mean total number of steps taken per day?

##### 1) Histogram of total number of steps taken each day


```r
library(ggplot2)
par(mai=c(1,1,1,1))
ggplot(activity, aes(x=date, y=steps)) + 
     geom_histogram(stat="identity") + 
     labs(x="Days", y="Steps", title="Total Steps/Day")
```

![plot of chunk histogram_totaldailysteps](figure/histogram_totaldailysteps.png) 

##### 2) Mean and Median total number of steps taken per day displayed in HTML format


```r
library(xtable)
meansteps <- mean(rowsum(activity$steps, activity$date), na.rm=TRUE)
mediansteps <- median(rowsum(activity$steps, activity$date), na.rm=TRUE)
statdf <- data.frame(cbind(meansteps, mediansteps))
names(statdf) <- c("mean", "median")
print(xtable(statdf), type="html", include.rownames=FALSE)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sat Oct 18 13:52:29 2014 -->
<table border=1>
<tr> <th> mean </th> <th> median </th>  </tr>
  <tr> <td align="right"> 10766.19 </td> <td align="right"> 10765.00 </td> </tr>
   </table>

## What is the average daily activity pattern?

##### 1) Time series plot of the 5 minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(ggplot2)
par(mai=c(1,1,1,1))
ggplot(activity, aes(x=interval, y=steps)) + 
     stat_summary(fun.y="mean", geom="line") + 
     labs(y="Average Steps", x="Interval", 
          title="Interval time Series of Avg number of Steps for all Days")
```

![plot of chunk interval_timeseries](figure/interval_timeseries.png) 

##### 2) The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps


```r
library(xtable)
t <- aggregate(steps ~ interval, data=activity, FUN="mean")
maxstepsdf <- data.frame(t[which.max(t$steps),]$interval)
names(maxstepsdf) <- c("Interval with Max Steps")
print(xtable(maxstepsdf), type="html", include.rownames=FALSE)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sat Oct 18 13:52:30 2014 -->
<table border=1>
<tr> <th> Interval with Max Steps </th>  </tr>
  <tr> <td align="right"> 835 </td> </tr>
   </table>

## Imputing missing values

##### 1) Total number of missing values in the dataset (i.e. the total number of rows with NAs) is `2304`

##### 2) Create a data frame that will contain steps at 5 minute interval mean across all days


```r
intervalmean <- aggregate(steps ~ interval, data=activity, FUN=mean)
```

##### 3) Make a copy of the original activity dataset called `nafilledactivity` dataset that has all the NAs filled in


```r
tmpdf <- merge(activity, intervalmean, by="interval")
nafilledactivity <- transform(tmpdf, steps=ifelse(is.na(steps.x), steps.y, steps.x))[c(5,3,1)]
```

**- Total number of missing values in the `NA filled` dataset (i.e. the total number of rows with NAs) is `0`**

##### 4) Histogram of the total number of steps for `NA filled` dataset


```r
library(ggplot2)
par(mai=c(1,1,1,1))
ggplot(nafilledactivity, aes(x=date, y=steps)) + 
     geom_histogram(stat="identity") + 
     labs(x="Days", y="Steps", title="Total Steps/Day")
```

![plot of chunk histogram_nafilled_totaldailysteps](figure/histogram_nafilled_totaldailysteps.png) 

##### 4) Mean and Median total number of steps taken per day for `NA filled` dataset displayed in HTML format


```r
library(xtable)
nafilledmeansteps <- mean(rowsum(nafilledactivity$steps, nafilledactivity$date))
nafilledmediansteps <- median(rowsum(nafilledactivity$steps, nafilledactivity$date))
nafilledstatdf <- data.frame(cbind(nafilledmeansteps, nafilledmediansteps))
names(nafilledstatdf) <- c("mean", "median")
print(xtable(nafilledstatdf), type="html", include.rownames=FALSE)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sat Oct 18 13:52:31 2014 -->
<table border=1>
<tr> <th> mean </th> <th> median </th>  </tr>
  <tr> <td align="right"> 10766.19 </td> <td align="right"> 10766.19 </td> </tr>
   </table>

##  

##### 4) The values between the `activity (original)` dataset and the `nafilledactivity (new)` dataset differ as shown in the table below.


```r
names(statdf) <- c("Original Mean", "Original Median")
names(nafilledstatdf) <- c("NA Filled Mean", "NA Filled Median")
print(xtable(cbind(statdf, nafilledstatdf)), type="html", include.rownames=FALSE)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sat Oct 18 13:52:31 2014 -->
<table border=1>
<tr> <th> Original Mean </th> <th> Original Median </th> <th> NA Filled Mean </th> <th> NA Filled Median </th>  </tr>
  <tr> <td align="right"> 10766.19 </td> <td align="right"> 10765.00 </td> <td align="right"> 10766.19 </td> <td align="right"> 10766.19 </td> </tr>
   </table>

##  

##### 4) The impact of imputing the missing data by the mean of 5 minute interval across all days  

      a) There were 8 days that had NA in the original activity data set
      b) The total steps for each of those 8 days will equal the computed mean of 5 minute interval across all days
      c) The mean remains the same


```r
tmp1 <- do.call("paste", nafilledactivity)
tmp2 <- do.call("paste", activity)
narows <- nafilledactivity[!tmp1 %in% tmp2,]
aggregate(steps ~ date, narows, sum)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-08 10766.19
## 3 2012-11-01 10766.19
## 4 2012-11-04 10766.19
## 5 2012-11-09 10766.19
## 6 2012-11-10 10766.19
## 7 2012-11-14 10766.19
## 8 2012-11-30 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

##### 1) Create a new factor variable in the dataset with two levels – `weekday` and `weekend` indicating whether a given date is a weekday or weekend day. Display the first few rows to show the `daytype` column being added


```r
weekdayabr <- weekdays(nafilledactivity$date, abbreviate=TRUE)
nafilled_activity_factor = 
     within(nafilledactivity, 
            {daytype = ifelse(weekdayabr == "Sat" | weekdayabr == "Sun", 
                              "weekend", "weekday")
            })
head(nafilled_activity_factor)
```

```
##      steps       date interval daytype
## 1 1.716981 2012-10-01        0 weekday
## 2 0.000000 2012-11-23        0 weekday
## 3 0.000000 2012-10-28        0 weekend
## 4 0.000000 2012-11-06        0 weekday
## 5 0.000000 2012-11-24        0 weekend
## 6 0.000000 2012-11-15        0 weekday
```

##### 2) Panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
library(ggplot2)
par(mai=c(1,1,1,1))
ggplot(nafilled_activity_factor, aes(x=interval, y=steps)) + 
     stat_summary(fun.y="mean", geom="line") + 
     labs(y="Average Steps", x="Interval", 
          title="Interval Time Series of Avg. Steps by Daytype") + 
     facet_wrap(~ daytype, nrow=2)
```

![plot of chunk panelplot_timeseries](figure/panelplot_timeseries.png) 

