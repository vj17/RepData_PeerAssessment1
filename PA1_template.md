# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data<-read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
#Compute total
total<-tapply(data$steps,data$date,sum)

#Plot histogram
hist(total,main="Histogram of Total number of steps per day",xlab="Total number of steps",col="orange")
```

![](PA1_template_files/figure-html/mean_totalsteps-1.png) 

```r
options("scipen"=999)

#Print Mean and Median
mean_total<-mean(total,na.rm=TRUE)
mean_total
```

```
## [1] 10766.19
```

```r
median_total<-median(total,na.rm=TRUE)
median_total
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
#Average steps grouped by interval
sub_data<-tapply(data$steps,data$interval,mean,na.rm=TRUE)
sub2_data<-as.data.frame(sub_data)

#Plot
plot(rownames(sub2_data),sub2_data[,1],type="l",main="Average daily activity pattern",xlab="Interval",ylab="Avg number of steps",col="magenta")
```

![](PA1_template_files/figure-html/avg_daily_activity-1.png) 

```r
#Find interval with max number of steps
which.max(sub2_data[,1])
```

```
## 835 
## 104
```
We can see that the interval 835 contains the maximum number of steps (Since tapply returns a list, we have the interval as the row name)

## Imputing missing values

```r
#Calculating total number of NAs
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
#Creating a duplicate set and filling the NAs with mean
new_data<-data
new_data[is.na(new_data)]<-mean_total

#Plotting a histogram
new_total<-tapply(new_data$steps,new_data$date,sum)
hist(new_total,main="Histogram of Total number of steps per day",xlab="Total number of steps",col="blue")
```

![](PA1_template_files/figure-html/input_NA-1.png) 

```r
#Mean and median
new_mean<-mean(new_total,na.rm=TRUE)
new_mean
```

```
## [1] 415998.5
```

```r
new_median<-median(new_total,na.rm=TRUE)
new_median
```

```
## [1] 11458
```
As we can see, the mean has changed by a significant value because of the presence of outliers while the median almost remains around the same value.

## Are there differences in activity patterns between weekdays and weekends?

```r
new_data["date"]<-as.Date(new_data$date)

#Creating factor variable
weekdays1<-c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
new_data$wday<-factor((weekdays(new_data$date) %in% weekdays1),levels=c(FALSE,TRUE),labels=c('weekend','weekday'))

library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#Filtering by weekday and weekend to calculate intervals separately
weekday_data<-filter(new_data,wday=="weekday")
weekend_data<-filter(new_data,wday=="weekend")

#Average steps grouped by interval
s1_data<-tapply(weekday_data$steps,weekday_data$interval,mean)
s2_data<-tapply(weekend_data$steps,weekend_data$interval,mean)

#Adding weekday and weekend columns
s1_data<-as.data.frame(s1_data)
s1_data$wday<-rep(c("weekday"),nrow(s1_data))
s2_data<-as.data.frame(s2_data)
s2_data$wday<-rep(c("weekend"),nrow(s2_data))

#Plot weekday data
plot(rownames(s1_data),s1_data[,1],type="l",col="red",main = "Average activity patterns on Weekdays",xlab="Interval",ylab="Avg number of steps")
```

![](PA1_template_files/figure-html/weekday_weekend-1.png) 

```r
#Plot weekend data
plot(rownames(s2_data),s2_data[,1],type="l",col="green",main = "Average activity patterns on Weekends",xlab="Interval",ylab="Avg number of steps")
```

![](PA1_template_files/figure-html/weekday_weekend-2.png) 
