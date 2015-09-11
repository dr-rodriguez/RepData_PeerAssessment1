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

# Change to log scale?
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

# Replace missing values
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
        print(newactivity$steps[i])
        print(newactivity$interval[i])
        print(interval5$mean[indexarray])
    }
}

sumdata <- 
    newactivity %>%
    group_by(date) %>%
    summarize(sum=sum(steps))

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
