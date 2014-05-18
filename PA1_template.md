Peer Assignment 1 - Edward Pittman
========================================================




```r
setwd("/Users/edward/Documents/R/Rcourse2014May")  #  set working directory
act1 = read.csv("activity.csv", header = TRUE)  #  read activity.csv file
act2 <- na.omit(act1)  # act2 excludes missing values

# install.packages('plyr') #installs plyr package
library(plyr)

# creates summary data frame act3 sums steps by day
act3 <- ddply(act2, ~date, summarize, stepsum = sum(steps))

# prints histogram of steps
hist(act3$stepsum, breaks = 15, main = "Histogram of Total Number of Steps Per Day", 
    ylab = "number of days", xlab = "total number of steps per day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```r

meanstep <- mean(act3$stepsum)  # calculate mean steps per day
medstep <- median(act3$stepsum)  # calulate median steps per day
```

The mean total number of steps per day is 1.0766 &times; 10<sup>4</sup>. The median number of total steps per day is 10765.

```r
# creates summary data frame act4 grouped by time of day
act4 <- ddply(act2, ~interval, summarize, stepmean = mean(steps))

plot.ts(act4$stepmean, type = "l")  #plots mean steps by time of day
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
tmp1 <- which.max(act4$stepmean)  #calculates time of day for max steps
maxtime <- act4[tmp1, 1]
```

The 5-minute interval with the maximum number of steps is 835.

```r
# creates act5 by merging act1 and act4 by interval to impute missing values
act5 <- merge(act1, act4, by = "interval", all = TRUE)
act5$ct1 <- ifelse(!is.na(act5$steps), 0, 1)  #1 if na
ctna <- sum(act5$ct1)  #count of na's
act5$steps2 <- ifelse(act5$ct1 == 1, act5$stepmean, act5$steps)

# creates summary data frame act5 sums steps by day
act6 <- ddply(act5, ~date, summarize, stepsum = sum(steps2))



# prints histogram of steps
hist(act6$stepsum, breaks = 15, main = "Histogram of Total Number of Steps Per Day \n     \n missing values imputed", 
    ylab = "number of days", xlab = "total number of steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
meanstepm <- mean(act6$stepsum)
# calculate mean steps per day w imputed missing
medstepm <- median(act6$stepsum)
# calulate median steps per day-imputes missing
```


The mean and median number of total steps per day with missing values imputed are 1.0766 &times; 10<sup>4</sup> and 1.0766 &times; 10<sup>4</sup> respectively. The mean values are the same with and without imputed data for missing values. The median with imputed data is slightly higher than the median excluding missing values.


```r
# creates categorical variable daytype indicating weekday or weekend
act5$x1 <- as.Date(act5$date)
act5$x2 <- weekdays(act5$x1)
act5$daytype <- "weekday"
act5$daytype <- ifelse(act5$x2 == "Saturday", "weekend", act5$daytype)
act5$daytype <- ifelse(act5$x2 == "Sunday", "weekend", act5$daytype)

# creates summary data frame act5 grouped daytype, then by time of day
act7 <- ddply(act5, .(daytype, interval), summarize, stepmean = mean(steps2))
library(ggplot2)
actwkd <- subset(act7, daytype == "weekday")
actwke <- subset(act7, daytype == "weekend")

plot.ts(actwkd$stepmean, type = "l", main = "weekday")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r

plot.ts(actwke$stepmean, type = "l", main = "weekend")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 








