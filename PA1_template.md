# Reproducible Research: Peer Assessment 1

This report makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## .
## Load required libraries and set the working directory.

```r
# Required library for data summaries.
require(plyr)
```

```
## Loading required package: plyr
```

```r
# Set working directory.
wd1 <- '/media/robert/OSDisk/Users/robert/Documents/Data/coursera_data_science/'
wd2 <- 'ch04_reproducible_research/week_01/PA1/RepData_PeerAssessment1'
wd <- paste(wd1, wd2, sep="", collapse="")
setwd(wd)
rm(wd, wd1, wd2)
```

## .
## Load, review, and preprocess the data.

Load the data if it is available.  Otherwise stop with an error.

```r
if (!file.exists("activity.zip")) {
    stop("activity.zip: File not found.  Check working directory and/or download file.")
    }
data <- read.csv(unz("activity.zip", "activity.csv"))
```

Review the dataset.

```r
summary(data)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

Preprocess to get summaries.

```r
# Weekday or weekend.
day <- weekdays(as.Date(data$date))
isWeekend <- day=="Saturday" | day=="Sunday"
data$dayOfWeek <- "weekday"
data$dayOfWeek[isWeekend] <- "weekend"
data$dayOfWeek <- as.factor(data$dayOfWeek)
rm(day, isWeekend)

# Total number of steps per day.
stepsByDate <- ddply(data[!is.na(data$steps),], "date", 
                     function(df) sum(df$steps))
names(stepsByDate) <- c("date", "sumSteps")

# Average number of steps per interval.
stepsByInterval <- ddply(data[!is.na(data$steps),], "interval", 
                         function(df) floor(mean(df$steps)))
names(stepsByInterval) <- c("interval", "avgSteps")
```


## .
## What is mean total number of steps taken per day?

The mean total number of steps per day is the summary across all intervals of each day in the study period.  Note, the missing data was removed by preprocessing.

```r
hist(stepsByDate$sumSteps, main="Histogram Total Steps per Day", 
     xlab="Steps per Day", col="wheat")
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

We can check the mean and median number of steps per day.

```r
# Using a dataframe to store the output.  In the rendered output it is supposed 
# to look like an annotated table, thus hopefully a little more readable. The
# dataframe, output, is used throughout this script.  Likewise for the temporary
# variable, x.
x <- floor(mean(stepsByDate$sumSteps))
output <- data.frame(MEASUREMENT="Mean", VALUE=x, UNITS="Steps", stringsAsFactors=FALSE)
x <- median(stepsByDate$sumSteps)
output <- rbind(output,c("Median", x, "Steps"))
output
```

```
##   MEASUREMENT VALUE UNITS
## 1        Mean 10766 Steps
## 2      Median 10765 Steps
```

## .
## What is the average daily activity pattern?
The average daily activity pattern is the summary of activity by interval across all days in the study.  Note, the missing data was removed by preprocessing.

```r
plot(stepsByInterval$interval, stepsByInterval$avgSteps, 
     main="Average Steps per Interval", xlab="Interval", ylab="Steps",
     type="l", col="darkgreen")
```

![plot of chunk unnamed-chunk-7](./PA1_template_files/figure-html/unnamed-chunk-7.png) 

Checking the maximum value below, we see it occurs at 8:35AM.

```r
x <- stepsByInterval$interval[stepsByInterval$avgSteps == max(stepsByInterval$avgSteps)]
output <- data.frame(MEASUREMENT="Time of Maximum Steps", VALUE=x, UNITS="Time", stringsAsFactors=FALSE) 
output
```

```
##             MEASUREMENT VALUE UNITS
## 1 Time of Maximum Steps   835  Time
```

## .
## Imputing missing values

We will now attempt to fill in the missing data and see the effect on the analysis.

The number of missing days and the percentage are shown below.  Note, from the summary of the dataset above, we know that the only missing values are in the variable "steps".

```r
# The number of NA's in the dataset.
x <- sum(is.na(data$steps))
output <- data.frame(MEASUREMENT="Number NA", VALUE=x, UNITS="Count", stringsAsFactors=FALSE) 

# The percentage of NA's in the dataset.
x <- floor(100 * sum(is.na(data$steps)) / length(data$steps))
output <- rbind(output,c("Percent NA", x, "Percent"))
output
```

```
##   MEASUREMENT VALUE   UNITS
## 1   Number NA  2304   Count
## 2  Percent NA    13 Percent
```

We need a strategy to fill in the missing data.  We have already seen how the number of steps vary across interval, let us look at the variability across days.

```r
plot(stepsByDate$date, stepsByDate$sumSteps, 
     main="Total Steps per Date", xlab="Date", ylab="Steps",
     type="l", col="darkgreen")
```

![plot of chunk unnamed-chunk-10](./PA1_template_files/figure-html/unnamed-chunk-10.png) 

The variability across days seems more randomly distributed compared to the variability across intervals.  A real statistician would start getting more technical.  Not me.  Let's go with interval.


```r
# Copy the data into a new dataframe.
dataImputed <- data

# Add the average intervals to the new dataframe.  Note, the stepsByInterval
# will be recycled to match the length of the new dataframe.
dataImputed$newSteps <- stepsByInterval$avgSteps

# Replace the NA's with the average values.
isNA <- is.na(dataImputed$steps)
dataImputed$steps[isNA] <- dataImputed$newSteps[isNA]

# Clear unneeded variables
rm(isNA)
dataImputed$newSteps <- NULL
```

Now we can see the effect of filling in the missing values.

```r
# Average number of steps per interval.
stepsByDateImputed <- ddply(dataImputed, "date", 
                            function(df) sum(df$steps))
names(stepsByDateImputed) <- c("date", "sumSteps")

hist(stepsByDateImputed$sumSteps, main="Histogram Total Steps per Day Imputed", 
     xlab="Steps per Day", col="wheat")
```

![plot of chunk unnamed-chunk-12](./PA1_template_files/figure-html/unnamed-chunk-12.png) 

Let us examine how the values have changed.

```r
# Imputed mean and median.
x <- floor(mean(stepsByDateImputed$sumSteps))
output <- data.frame(MEASUREMENT="Mean Imputed", VALUE=x, UNITS="Steps", stringsAsFactors=FALSE) 
x <- median(stepsByDateImputed$sumSteps)
output <- rbind(output, c("Median Imputed", x, "Steps"))

# The percent differences with respect to the original data set.
x <- 100 * (mean(stepsByDateImputed$sumSteps) - mean(stepsByDate$sumSteps)) / 
    mean(stepsByDate$sumSteps)
output <- rbind(output, c("Mean Percent Change", round(x, digits=2), "Percent"))
x <- 100 * (median(stepsByDateImputed$sumSteps) - median(stepsByDate$sumSteps)) / 
    median(stepsByDate$sumSteps)
output <- rbind(output, c("Median Percent Change", round(x, digits=2), "Percent"))
output
```

```
##             MEASUREMENT VALUE   UNITS
## 1          Mean Imputed 10749   Steps
## 2        Median Imputed 10641   Steps
## 3   Mean Percent Change -0.15 Percent
## 4 Median Percent Change -1.15 Percent
```

So, less than 2% difference with the imputed values.  Let us look at weekends vs. weekdays.

## .
## Are there differences in activity patterns between weekdays and weekends?

Preprocess the data to get summary information.

```r
# Average number of steps per interval per dayOfWeek.
stepsByIntervalImputed <- ddply(dataImputed, c("interval","dayOfWeek"), 
                                function(df) floor(mean(df$steps)))
names(stepsByIntervalImputed) <- c("interval", "dayOfWeek", "avgSteps")
```

Plot the number of steps per interval for weekdays and weekends.

```r
# Arcane formatting parameters.
split = .41
par(fig=c(0,1,split,1))
par(mar=c(4,4,2,2)+.1)

temp <- stepsByIntervalImputed[stepsByIntervalImputed$dayOfWeek=="weekend",]
weekendAverage <- mean(temp$avgSteps)
plot(temp$interval, temp$avgSteps, type="l", main="Weekend", 
         xlab="", ylab="Steps", axes=FALSE)
axis(2)
box()

par(fig=c(0,1,0,1-split), new=TRUE)

temp <- stepsByIntervalImputed[stepsByIntervalImputed$dayOfWeek=="weekday",]
weekdayAverage <- mean(temp$avgSteps)
plot(temp$interval, temp$avgSteps, type="l", main="Weekday", 
         xlab="Interval", ylab="Steps")
```

![plot of chunk unnamed-chunk-15](./PA1_template_files/figure-html/unnamed-chunk-15.png) 

Clearly the distribution is different.  Weekdays start with a jolt a little after 5:00AM, then taper off significantly a bit before 10:00AM.  Weekends ramp up gradually before 9:00AM, then keep a more steady pace until 8:00PM or so.

How about averages?  We calculated them above, let us examine them now.

```r
x <- round(weekendAverage, digits=2)
output <- data.frame(MEASUREMENT="Weekend Average", VALUE=x, UNITS="Steps", stringsAsFactors=FALSE) 
x <- round(weekdayAverage, digits=2)
output <- rbind(output, c("Weekday Average", x, "Steps"))
output
```

```
##       MEASUREMENT VALUE UNITS
## 1 Weekend Average 41.94 Steps
## 2 Weekday Average 35.12 Steps
```

In this study at least, the average steps per five minute interval is higher during the weekend than the weekday.

## .
## Conclusion

Walking more makes you healthier.  People, according to this study, walk more on weekends.  So, for the good of all people, we must institute 2-day work weeks and a 5-day weekends!

## .