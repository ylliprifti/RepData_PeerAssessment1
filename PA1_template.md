# Reproducible Research: Peer Assessment Ylli


## Loading and preprocessing the data


```r
library(knitr)
library(data.table)
#library(ggplot2)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:data.table':
## 
##     hour, mday, month, quarter, wday, week, yday, year
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
activity <- data.table(read.csv("activity.csv"))      #read csv file
activity$date <- ymd(activity$date)                   #set date format
summary <- activity[, list(sumSteps = sum(steps, na.rm = TRUE)), by=c("date")] #create summary with sum
summary <- summary[sumSteps > 0 & !is.nan(sumSteps)]  #remove days with 0 steps
```


## What is mean total number of steps taken per day?


```r
hist(x=summary$sumSteps, xlab = "Total number of Steps", ylab = "Frequency", main = "Number of Steps Frequency", col = "red", breaks = 20)
```

![](PA1_template_files/figure-html/simulate_data_1-1.png)<!-- -->

### The mean number of steps per day is: ``10766``, the median is: ``10765``.


## What is the average daily activity pattern?


```r
  summary2 <- activity[, list(meanSteps = mean(steps, na.rm = TRUE)), by=c("interval")]

  with(summary2, plot(x=interval, y=meanSteps, type = "l", main="Time Series 5-minutes interval", xlab="5-minute interval", ylab="Average number of steps"))
```

![](PA1_template_files/figure-html/simulate_data_2-1.png)<!-- -->

### The interval with the maximum number of steps is the following: 


```r
  names(summary2) <- c("interval", "steps")
  knitr::kable(summary2[which.max(summary2$steps), ])
```



 interval      steps
---------  ---------
      835   206.1698

## Imputing missing values

### The total number of missing values is: ``2304``

### The mean for the day will be used to calculate the steps for the missing values


```r
  names(summary2)       <- c("interval", "meanSteps")                          #reset names for summary2

  missing               <- activity[is.na(steps)]                              #missing data
  nonmissing            <- merge(activity, summary2, by="interval", sort=FALSE)#add mean by interval
  missing$steps         <- nonmissing[is.na(steps), ]$meanSteps                #calculate missing steps
  nonmissing$meanSteps  <- NULL                                                #remove unised column
  
  nonmissing            <- nonmissing[!is.na(steps), ]                         #only non missing data
  nonmissing            <- rbind(missing, nonmissing)                          #append missing data
  nonmissing            <- nonmissing[order(date,interval)]                    #order by date, interval
  
  
  knitr::kable(head(nonmissing))
```

     steps  date          interval
----------  -----------  ---------
 1.7169811  2012-10-01           0
 0.3396226  2012-10-01           5
 0.1320755  2012-10-01          10
 0.1509434  2012-10-01          15
 0.0754717  2012-10-01          20
 2.0943396  2012-10-01          25

```r
  par(mfrow=c(1,2))
  
  
  hist(x=summary$sumSteps, xlab = "Total number of Steps", ylab = "Frequency", main = "Before", col = "red", breaks = 20)
  
  summary3 <- nonmissing[, list(sumSteps = sum(steps, na.rm = TRUE)), by=c("date")] #summary with sum
  summary3 <- summary3[sumSteps > 0 & !is.nan(sumSteps)]  #remove days with 0 steps
  
  hist(x=summary3$sumSteps, xlab = "Total number of Steps", ylab = "Frequency", main = "After", col = "red", breaks = 20)
```

![](PA1_template_files/figure-html/simulate_data_3-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?


```r
  wdActivity         <- nonmissing[wday(date) <=5, list(meanSteps = mean(steps)), by=c("interval")] #mean steps for weekdays
  weActivity         <- nonmissing[wday(date) >5,  list(meanSteps = mean(steps)), by=c("interval")] #mean steps for weekends
  
  wdActivity$weekday <- 'weekday' #mark with factor value 
  weActivity$weekday <- 'weekend' #mark with factor value
  
  weekActivity       <- rbind(wdActivity, weActivity) #merge records
  
  library(lattice)
  xyplot(weekActivity$meanSteps ~ weekActivity$interval | weekActivity$weekday, layout = c(1,2), type="l", xlab = "Interval", ylab="Steps")
```

![](PA1_template_files/figure-html/simualte_data_4-1.png)<!-- -->
