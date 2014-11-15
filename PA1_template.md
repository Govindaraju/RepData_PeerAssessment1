# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

```r
library(data.table)
data <- data.table(read.csv(file="activity.csv"))
data$date <- as.Date(as.character(data$date),"%Y-%m-%d")
```
## What is mean total number of steps taken per day?

```r
#res <- data[,sum(steps,na.rm=T),by=date]
#data.na <- data[!is.na(data$steps),]
datasum   <- aggregate(steps ~ date,data=data,FUN="sum")
hist(datasum$steps,xlab = "Total steps each day",main="Histogram of total number of steps taken each day")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

```r
meanvalue = mean(datasum$steps)
medianvalue = median(datasum$steps)
print(paste("Mean value of steps perday",meanvalue))
```

```
## [1] "Mean value of steps perday 10766.1886792453"
```

```r
print(paste("Median value of steps perday",medianvalue))
```

```
## [1] "Median value of steps perday 10765"
```


## What is the average daily activity pattern?

```r
datamean <- aggregate(steps ~ interval,data=data,FUN="mean")
# intervals are in millitary format, convert into standard format
# for eg 2015 will become 20.10 which is 20 hrs and 15 minuts, in numerical
# term this 20.25 (modulus and quotient operators are used)
ivl <- datamean$interval
ivl <- ivl %/% 100 + (ivl %% 100)/60

x <- ivl
y <- datamean$steps

 plot(x,y,type="l",xlab = "Intervals in hours", ylab="Steps",main="Average steps of all days based on intervals")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

```r
maxav <- datamean[datamean$steps == max(datamean$steps),]
print(paste("Interval having the maximum steps on an average of all days",maxav$interval))
```

```
## [1] "Interval having the maximum steps on an average of all days 835"
```
## Imputing missing values

```r
nacases <- function(dat){
  totalrows <- nrow(dat)
  completerows <- nrow(dat[complete.cases(dat),])
  rowswithmissingvalues <- totalrows - completerows
}
print(paste("rows with NA values in data = ",nacases(data)))
```

```
## [1] "rows with NA values in data =  2304"
```

```r
# replacing all NA values with the mean, which is 0
data.new <- data

steps <- data.new$steps
na <- is.na(steps)
mean <- mean(steps,na.rm=T) # mean is 0, so all NAs will be replaced by 0
steps[na] = mean

#now replace steps in the data with the na removed step(which is done above)
data.new$steps = steps

print(paste("rows with NA values in data.new = ",nacases(data.new)))
```

```
## [1] "rows with NA values in data.new =  0"
```

```r
datasum.new   <- aggregate(steps ~ date,data=data.new,FUN="sum")
hist(datasum.new$steps,xlab = "Total steps each day",main="Histogram of total no of steps taken each day(na removed)")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

```r
meanvalue.new = mean(datasum.new$steps)
medianvalue.new = median(datasum.new$steps)
print(paste("Mean value of steps perday",meanvalue.new))
```

```
## [1] "Mean value of steps perday 10766.1886792453"
```

```r
print(paste("Median value of steps perday",medianvalue.new))
```

```
## [1] "Median value of steps perday 10766.1886792453"
```
<font color="green">Mean value remain the same before and after imputing where as the median value is different. This is obvious from the data printed above.</font>

<font color ="green">
Before removing NA - dates with valid total number of steps =
</font>


```r
nrow(datasum)
```

```
## [1] 53
```

<font color ="green">
After removing NA - dates with valid total number of steps =
</font>


```r
nrow(datasum.new)
```

```
## [1] 61
```

<font color ="green">
The number of rows itself varies between imputed and un-imputed data.
Also having a close look at the data will further reveal, imputing has 
a significant impact on 8 records.
</font>

## Are there differences in activity patterns between weekdays and weekends?
