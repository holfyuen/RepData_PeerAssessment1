# Reproducible Research - Assignment 1
Holf Yuen

## Loading and preprocessing the data


```r
data<-read.csv("activity.csv")
data$date <- as.character(data$date)
data$date <- as.Date(data$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```r
library(dplyr)
total_steps<- data %>% filter(!is.na(steps)) %>% group_by(date) %>% summarise(sum(steps)) %>% as.data.frame()
hist(total_steps$`sum(steps)`,xlab="Steps",main="Total no. of steps each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
meanst<- as.integer(mean(total_steps$`sum(steps)`))
medst<-median(total_steps$`sum(steps)`)
```

The mean of the total number of steps taken per day is 10766.  
The median of the total number of steps taken per day is 10765.

## What is the average daily activity pattern?


```r
avg_steps<- data %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarise(mean(steps)) %>% as.data.frame()
with(avg_steps,plot(interval,`mean(steps)`,type="l"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
maxstep<-avg_steps[which.max(avg_steps$`mean(steps)`),1]
```

835 is the interval of maximum number of steps

## Input missing values


```r
narow<-sum(is.na(data$steps))
# Filling in missing values with means of 5-minute intervals
data_2<-left_join(avg_steps,data,by="interval")
data_2a<-data_2 %>% filter(is.na(steps)) %>% select(date,interval,`mean(steps)`)
data_2b<-data_2 %>% filter(!is.na(steps)) %>% select(date,interval,steps)
colnames(data_2a)[3]<-"steps"
data_2f<-rbind(data_2a,data_2b)
# Calculate total number of steps
total_steps2<- data_2f %>% group_by(date) %>% summarise(sum(steps)) %>% as.data.frame()
hist(total_steps2$`sum(steps)`,xlab="Steps",main="Total no. of steps each day (with NAs filled)")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
meanst2<- as.integer(mean(total_steps2$`sum(steps)`))
medst2<-as.integer(median(total_steps2$`sum(steps)`))
```

Missing values: 2304.  
With NA values filled,  
The mean of the total number of steps taken per day is 10766.  
The median of the total number of steps taken per day is 10766.

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lubridate)
WD<-wday(data_2f$date) #Weekends are 7 and 1
WDF<-factor(WD,labels=c("Weekend",rep("Weekday",5),"Weekend"))
```

```
## Warning in `levels<-`(`*tmp*`, value = if (nl == nL) as.character(labels)
## else paste0(labels, : duplicated levels in factors are deprecated
```

```r
WDF2<-factor(WDF,levels=c("Weekday","Weekend"))
data_2fa<-cbind(data_2f,"DayofWeek"=WDF2)
avg_steps2<- data_2fa %>% group_by(DayofWeek,interval) %>% summarise(mean(steps)) %>% as.data.frame()
library(ggplot2)
qplot(interval,`mean(steps)`,data=avg_steps2,facets=DayofWeek~.,geom="line",xlab="Interval",ylab="Mean no. of Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
