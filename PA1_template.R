#Load and process the data.
data <- read.csv("/Users/dmoney/Desktop/R/activity.csv", header = T)

#Load the packages I will be using.
library(dplyr)
library(lattice)

#Recreate the interval variable in POSIXct.
#This will be useful for plotting later on.
hours <- NULL
for(i in 1:24){
        h1 <- 0:23
        h2 <- rep(h1[i], times = 12)
        hours <- c(hours, h2)
}
minutes <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)
posix <- paste(hours, minutes, sep = ":")
posix <- strptime(posix, format = "%H:%M")
posix <- as.POSIXct(posix)
data <- cbind(data, posix = posix)

#Total number of steps taken per day.
totalsteps <- aggregate(steps ~ date, data = data, sum) %>%
        rename(total_daily_steps = steps)

#Create historgram of steps taken each day.
hist(totalsteps$total_daily_steps, 
     xlab = "Steps", 
     main = "Total Number of Steps Taken Each Day",
     breaks = 40)

#Calculate mean and median of total number of steps per day.
mean(totalsteps$total_daily_steps)
median(totalsteps$total_daily_steps)

#Reshape data to calculate the number of Steps Per Interval (SPI).
SPI <- aggregate(steps ~ interval, data = data, mean) %>%
        rename(avg_steps = steps) %>%
        cbind(posix) %>%
        select(avg_steps, interval, posix)

#Create time series plot.
plot(SPI$posix, SPI$avg_steps, 
     type = "l",
     xlab = "Interval",
     ylab = "Number of steps",
     main = "Avg. Number of Steps per 5-Min Interval")

#Find 5-minute interval with max steps
filter(SPI, avg_steps == max(avg_steps))[[1, 2]]

#Find total number of missing values in dataset
sum(is.na(data))

#Fill in missing data with avg steps taken for that 5-minute interval
#Crate new dataset with missing data filled in
newdata <- data
for(i in 1:17568){
        newdata$steps[i] <- if(is.na(data$steps[i]) == TRUE){
                subset(SPI, SPI$interval == data$interval[i])[[1]]                                  
        }else{data$steps[i]}
}

#Reshape new data.
newtotalsteps <- aggregate(steps ~ date, data = newdata, sum) %>%
        rename(total_daily_steps = steps)

#Create histogram total steps taken each day using new dataset
hist(newtotalsteps$total_daily_steps,
     xlab = "Steps", 
     main = "Total Number of Steps Taken Each Day (NAs Replaced with Avg)",
     breaks = 40)

#Calculate mean and median of total number of steps per day for new data
mean(newtotalsteps$total_daily_steps)
median(newtotalsteps$total_daily_steps)

#Create new variable "day_type" with two levels: "Weekday" and "Weekend"
day_type_FUN <- function(x){
        ifelse(x %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
               "Weekday", ifelse(x %in% c("Saturday", "Sunday"), 
                                 "Weekend", "not_valid_day"))
}
day_of_week <- as.POSIXlt(newdata$date)
day_of_week <- weekdays(day_of_week)
newdata <- cbind(newdata, day_of_week)
day_type <- day_type_FUN(day_of_week)
newdata <- cbind(newdata, day_type)
newdata$day_type <- factor(newdata$day_type)

#Calculate the number of Steps Per Interval for new data for Weekday & Weekend
#reshape data for plotting
newdata_weekday <- filter(newdata, day_type == "Weekday")
newdata_weekend <- filter(newdata, day_type == "Weekend")
newSPI_weekday <- aggregate(steps ~ interval, data = newdata_weekday, mean) %>%
        mutate(day_type = "Weekday")
newSPI_weekend <- aggregate(steps ~ interval, data = newdata_weekend, mean) %>%
        mutate(day_type = "Weekend")
newSPI <- rbind(newSPI_weekday, newSPI_weekend)
newSPI$day_type <- factor(newSPI$day_type)
newSPI <- cbind(newSPI, posix = posix)

#create plotting margins and label
at <- c(newSPI$posix[1],
        newSPI$posix[61],
        newSPI$posix[121],
        newSPI$posix[181],
        newSPI$posix[241])
labels <- c("00:00", "05:00", "10:00", "15:00", "20:00")

#plot the data
xyplot(steps ~ posix | day_type, data = newSPI,
       type = "l",
       xlab = "Interval",
       ylab = "Number of steps",
       layout = c(1, 2),
       scales = list(x=list(at = at, labels = labels)))