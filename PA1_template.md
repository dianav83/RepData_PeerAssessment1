# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
setInternet2(use = TRUE)
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activitydata.zip")
unzip("activitydata.zip")
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
activity1 <- aggregate(. ~ date, data = activity[,1:2], FUN = sum)
hist(x = activity1$steps, xlab = "Total Steps per Day", main = "Histogram of Total Steps per Day")
```

![](PA1_template_files/figure-html/mean steps-1.png) 

```r
summary(activity1$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

## What is the average daily activity pattern?

```r
activity2 <- aggregate(. ~ interval, data = activity[,c(1,3)], FUN = mean)
plot(activity2, type = "l", main = "Average Daily Activity Pattern", xlab = "Interval", xaxp = c(0, 2500, 50))
```

![](PA1_template_files/figure-html/daily activity-1.png) 

```r
activity2[which.max(activity2$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

```r
sum(is.na(activity$steps)) ##Calculate the number of NAs
```

```
## [1] 2304
```

```r
ac3 <- merge(activity, activity2, by = "interval")##Fill in the missing values
colnames(ac3) <- c("interval", "xsteps", "date", "ysteps")
ac3$xsteps[is.na(ac3$xsteps)] <- ac3$ysteps[is.na(ac3$xsteps)]
activity3 <- ac3[,1:3]##create data set equal to original but with filled in NAs
act3 <- aggregate(. ~ date, data = activity3[,2:3], FUN = sum)
hist(x = act3$xsteps, xlab = "Total Steps per Day", main = "Histogram of Total Steps per Day (NA removed)")
```

![](PA1_template_files/figure-html/missing values-1.png) 

```r
summary(act3$xsteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

## Are there differences in activity patterns between weekdays and weekends?

```r
activity3$day <- ifelse(weekdays(as.Date(activity3$date)) %in% c('Saturday','Sunday'), "weekend", "weekday")
act4 <- aggregate(xsteps ~ interval*day, data = activity3, FUN = mean)
library(ggplot2)
p <- ggplot(act4, aes(x = interval, y = xsteps)) + geom_line() 
p + facet_grid(day~.)
```

![](PA1_template_files/figure-html/weekdays/weekends-1.png) 
