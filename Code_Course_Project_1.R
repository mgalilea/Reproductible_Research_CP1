# Code project version 2
d <- read.csv("./activity.csv",colClasses = c("numeric","Date","numeric"))
dsum <- tapply(d$steps,d$date,sum,na.rm=TRUE)
hist(dsum,col="red",xlab="Total Steps by Date",main="Histogram of Total Steps by Date")
mean(dsum)
median(dsum)
daverage <- tapply(d$steps,d$interval,mean,na.rm=TRUE)
plot(names(daverage),daverage,type="l",xlab="5-minute interval",ylab="Average number of steps",main="Time Series Plot of 5-min Interval Average",col="blue")
max_avg <- max(daverage)
n_match <- match(max_avg,daverage)
daverage[n_match]
sum(is.na(d))
dincomplete <- d[is.na(d),]
dcomplete <- d[complete.cases(d),]
dincomplete$steps <- as.numeric(daverage)
d1 <- rbind(dincomplete,dcomplete)
dsum1 <- tapply(d1$steps,d1$date,sum,na.rm=TRUE)
hist(dsum1,col="blue",xlab="Total Steps by Date",main="Histogram of Total Steps by Date with no NA values")
mean(dsum1)
median(dsum1)
weekenddays <- c("sábado","domingo")
d1$weekday <- factor((weekdays(d1$date) %in% weekenddays),levels=c(TRUE,FALSE),labels=c("weekend","weekday"))
d1weekdays <- d1$weekday == "weekday"
d1weekend <- d1$weekday == "weekend"
daverageweekday <- tapply(d1$steps[d1weekdays],as.factor(d1$interval[d1weekdays]),mean,na.rm=TRUE)
daverageweekend <- tapply(d1$steps[d1weekend],as.factor(d1$interval[d1weekend]),mean,na.rm=TRUE)
par(mfcol = c(2, 1))
plot(names(daverageweekday), daverageweekday, type = "l", xlab = "5 Min interval", ylab = "Average number of steps", main = "Average number of steps per interval Weekdays", 
     ylim = range(0:250), xlim = range(0:2400))
plot(names(daverageweekend), daverageweekend, type = "l", xlab = "5 Min interval", ylab = "Average number of steps", main = "Average number of steps per interval Weekend Days", 
     ylim = range(0:250), xlim = range(0:2400))