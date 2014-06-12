# Reproducible Research
## Peer Assignment 1

Let's start with loading the data

```r
library(knitr)
Data<-read.csv("activity.csv",header=T)
Data$date<-as.Date(Data$date,format="%Y-%m-%d")
cleanData<-na.omit(Data)
```

The first question is: What is mean total number of steps taken per day?
Here is a histogram:

```r
total<-by(cleanData$steps,cleanData$date,sum)
plot(total~unique(cleanData$date),type="h", main = "Sum of the steps per day", xlab="Date", ylab="Sum of steps", lwd = 7, col="gray")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean<-mean(total)
median<-median(total)
```
The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>, and the median is 10765. They are almost equal, which means that the distribution of steps per day is not skewed.

The second question is: What is the average daily activity pattern?

```r
intervals<-unique(cleanData$interval)
a<-split(cleanData$steps, intervals)
avg<-sapply(a, mean)
toPlot<-as.data.frame(cbind(intervals,avg), row.names=NULL)
plot(toPlot$avg~toPlot$intervals,type="l", main = "Average daily activity pattern", xlab="Interval", ylab="Sum of steps")
abline(h=mean(toPlot$avg), col="red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
maxInt<-toPlot[toPlot$avg==max(avg), 1]
```
The plot show that the interval with highest average number of steps taken is 835. The red line represents average dayly activity for the investigated period of time.

The next task is to fill the missing values.

```r
missing<-is.na(Data)
n.missing<-sum(missing)
```
The total number of row that contain missing values are 2304.
I chose to substitude the missing values with a random data point from the dataset.

```r
set.seed(1)
random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}
imputedSteps<-random.imp(Data$steps)
impData<-as.data.frame(cbind(imputedSteps,Data$date,Data$interval))
names<-colnames(Data)
colnames(impData)<-names
impData$date<-as.Date(impData$date,format="%Y-%m-%d",origin="1970-01-01")
Total<-by(impData$steps,impData$date,sum)
plot(Total~unique(impData$date),type="h", main = "Total number of steps per day", xlab="Date", ylab="Sum of steps", lwd = 7, col="gray")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
Mean<-mean(Total)
Median<-median(Total)
```
The above plot is a copy of the firt one, but here missing values were imputed. As a result the dayly mean is 1.0769 &times; 10<sup>4</sup> and the dayly median is 1.0765 &times; 10<sup>4</sup>. As a result the mean is almost the same(the diference is in the 5th decimal space), while the median has stayed the same.

The last question is: Are there differences in activity patterns between weekdays and weekends?
First we need to add a weekday/weekend filter. Then create a plot of the average number of sterps per 5-minute interval across weekends and weekdays.

```r
week<-weekdays(impData$date)
weekFilter<-ifelse(week %in% c("Saurday", "Sunday"), "weekend", "weekday")
LastData<-as.data.frame(cbind(impData,weekFilter))
wd<-subset(LastData, LastData$weekFilter=="weekday")
we<-subset(LastData, LastData$weekFilter=="weekend")
wdS<-split(wd$steps, intervals)
avgWD<-sapply(wdS, mean)
weS<-split(we$steps, intervals)
avgWE<-sapply(weS, mean)
final<-as.data.frame(cbind(intervals,avgWD, avgWE), row.names=NULL)
colnames(final)<-c("interval","weekday","weekend")
par(mfcol=c(2, 1))
plot(final$weekday~final$interval, type="l",main = "Average daily activity pattern: Weekdays", xlab="Interval", ylab="Average number of steps" )
abline(h=mean(final$weekday), col="red")
plot(final$weekend~final$interval, type="l",,main = "Average daily activity pattern: Weekend", xlab="Interval", ylab="Average number of steps" )
abline(h=mean(final$weekend), col="red")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

The mean for each plot is represented with a red line. Not only that it lies higher for weekends than for weekdays, but other differences could also be observed.
