---
title: "Reproducible Research_Course Project 1"
author: "Amir Ansaripour"
date: "January 10, 2016"
output: html_document
---

This is a report regarding a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

This study have aimed to investigate the average daily activity pattern and differences in activity patterns between weekdays and weekends.

######################
The different steps of the analysis have been listed in the following orders: 

1- setting the target directory

```{r}
setwd("D:/R courses/Reproducible Research")
rm(list=ls(all=TRUE))
```

2- reading data set:

```{r}
class = c("integer", "character", "integer")
Act <- read.csv("activity.csv", header = TRUE, sep="," , colClasses=class, na.strings="NA")
```

What is mean total number of steps taken per day?

3- the data aggregated by date and a histogram produced displaying the data.
This histogram has been divided into 20 buckets arbitrarily chosen. 

```{r}
Act_steps_day <- aggregate(steps ~ date, Act,sum,na.rm=TRUE)

hist(Act_steps_day$steps,breaks=20,col="red",xlab="Steps", ylab="Frequency",
        main="The distribution of daily total steps")
```

4- calculating the mean and median total number of steps taken per day

```{r}
steps_mean <- mean(Act_steps_day$steps, na.rm=TRUE)
steps_mean
steps_median <- median(Act_steps_day$steps, na.rm=TRUE)
steps_median

```

What is the average daily activity pattern?

5- I aggregated the data on steps by the interval and calculated the mean. 
then I used obtained information to plot a graph. The maximum value finds on the graph easily.


```{r}
Act_steps <- aggregate(steps ~ interval, Act, mean, na.rm=TRUE)
plot(Act_steps$interval, Act_steps$steps, type="l", col="red", main="Average Steps/5 Mins",
     xlab="Interval", ylab="steps")
```

6- Then, I investigated on average, which 5-minute interval represents the maximum number of steps:

```{r}
steps_max <- max(Act_steps$steps)
steps_max
Act_steps[Act_steps$steps == steps_max, ]
```

Imputing missing values

6- First of all, I calculated the total number of missing values:

```{r}
Act_step_nas <- sum(is.na(Act$steps))
Act_step_nas
```


7- I chose to replace NAs with the mean value in the data set. Later on, the mean and median for new data set calculated.

```{r}
Actimproved <- Act
Actimproved[is.na(Act$steps),]$steps<- mean(Act$steps, na.rm=TRUE)
Actimproved <- aggregate(steps ~ date, data=Actimproved, sum, na.rm=TRUE)
hist(Actimproved$steps, breaks=20,col="red", main="Total Steps per Day \n Adjusted Data",
     xlab="Steps", ylab="Frequency")
```


```{r}
imp_steps_mean <- mean(Actimproved$steps)
imp_steps_mean
imp_steps_median <- median(Actimproved$steps)
imp_steps_median
```

Are there differences in activity patterns between weekdays and weekends?

In order to quickly determine if there are activity differences between weekends and weekdays we simply plot two graphs, one with weekday data and one with weekend data. By plotting one above the other our x-axes are identical enabling us to "eyeball" (a time-honored scientific method) the graphs to note any differences.

8- I, firstly, tried to convert date to week days variable. then, new data set including the "weekdays" aggregated on steps again.

```{r}
Actimproved <- Act
Actimproved[is.na(Act$steps),]$steps<- mean(Act$steps, na.rm=TRUE)
Actimproved$date <- as.Date(Actimproved$date)
Actimproved$weekdays <- weekdays(Actimproved$date)
Actimproved$weekend <- as.factor(ifelse(Actimproved$weekdays=="Saturday"|Actimproved$weekdays=="Sunday","weekend","weekday"))
Diff_days <- aggregate(steps ~ interval+weekend, Actimproved, mean)
```

9- I made a plot containing a time series of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.
```{r}
library(ggplot2)
library(plyr)

p <- ggplot(Diff_days, aes(x=interval, y = steps)) +       
  geom_line(aes(group=weekend,col=weekend)) + xlab("interval") + ylab(expression("Steps")) +
  ggtitle("The average number of steps across weekdays and weekends")
p
```


Conclusion:

The results show a higher frequency counts specifically at the center region (near to the mean) as a results of imputing missing data on the estimates of the total daily number.

Moreover, the activities over the weekdays arise earlier than the weekends - activities on weekdays arise around  6 AM and activities arise around 8am on  weekends. On the other hand, the weekends have higher activities than the weekdays beyond 10am to 5pm.
















