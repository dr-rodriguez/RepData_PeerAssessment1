# Script to test out things before adding to the R Markdown file

# ========================
activity <- read.csv('activity.csv', stringsAsFactors=T)
library(dplyr)

# ========================
sumdata <- 
    activity %>%
    group_by(date) %>%
    summarize(sum=sum(steps, na.rm=T))
    

library(ggplot2)

p <- ggplot(sumdata, aes(date, sum)) + 
    geom_bar(fill='darkgreen', col='white', stat="identity") +
    labs(x='Date', y='Total Number of Steps') + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle=90))

print(p)
#ggsave('figures/plot1.png')

# Numeric output required in step
mean(sumdata$sum)
median(sumdata$sum)

# ========================
# Group by interval and take the average of the step, 
# then plot interval and step average
interval5 <-
    activity %>%
    mutate(interval=as.factor(interval)) %>%
    group_by(interval) %>%
    summarize(mean=mean(steps, na.rm=T)) %>%
    mutate(interval=as.integer(as.character(interval)))


p <- ggplot(interval5, aes(x=interval, y=mean)) +
    geom_line(col='blue', size=1) + 
    labs(x='Time Interval', y='Average Number of Steps') + 
    theme_bw()

print(p)

# Find the maximum
interval5 %>%
    filter(mean==max(mean, na.rm=T)) %>%
    select(interval, mean)

# ========================
# Impute missing values

# Number of missing values
activity %>%
    filter(is.na(steps)) %>%
    summarize(countNA=n())

# Replace missing values (option 1)
# Use average value from intervals
interval5 <-
    activity %>%
    mutate(interval=as.factor(interval)) %>%
    group_by(interval) %>%
    summarize(mean=mean(steps, na.rm=T)) %>%
    mutate(interval=as.integer(as.character(interval)))

newactivity <- activity
for(i in seq_along(newactivity$steps)) {
    if(is.na(newactivity$steps[i])) {
        indexarray <- interval5$interval == newactivity$interval[i]
        newactivity$steps[i] <- interval5$mean[indexarray]
        #print(newactivity$steps[i])
        #print(newactivity$interval[i])
        #print(interval5$mean[indexarray])
    }
}

sumdata2 <- 
    newactivity %>%
    group_by(date) %>%
    summarize(sum=sum(steps))

ggplot(sumdata2, aes(date, sum)) + 
    geom_bar(fill='darkred', col='white', stat="identity") +
    labs(x='Date', y='Total Number of Steps') + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle=90))

# Numeric output required in step
mean(sumdata2$sum)
median(sumdata2$sum)

# Some comparisons
activity %>% filter(date=='2012-11-01')
newactivity %>% filter(date=='2012-11-01')
sumdata2 %>% filter(date=='2012-11-01')
# Problem: because of averaging by interval, each day with NA has same values (10766.19)

# Replace missing values (option 2)
# Interpolate between days
sumdata <- 
    activity %>%
    group_by(date) %>%
    summarize(sum=sum(steps))
# Values with 0 had NA values for the whole day

# Loop through and interpolate around 0 values
sumdata2 <- sumdata
len <- nrow(sumdata2)
for(i in seq_along(sumdata2$sum)) {
    # Select 0 values
    if(sumdata2$sum[i] == 0) {
        print(i)
        # First element
        if(i==1) {
            sumdata2$sum[i] <- sumdata2$sum[i+1]/2
            next
        }
        # Last element
        if(i==len) {
            sumdata2$sum[i] <- sumdata2$sum[i-1]/2
            next
        }
        # Normal element
        print(sumdata2$sum[i])
        avg <- (sumdata2$sum[i-1] + sumdata2$sum[i+1])/2
        sumdata2$sum[i] <- avg
    }
}

ggplot(sumdata2, aes(date, sum)) + 
    geom_bar(fill='darkred', col='white', stat="identity") +
    labs(x='Date', y='Total Number of Steps') + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle=90))

# Numeric output required in step
mean(sumdata2$sum)
median(sumdata2$sum)
