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
interval5 <-
    activity %>%
    mutate(interval=as.factor(interval)) %>%
    group_by(interval) %>%
    summarize(mean=mean(steps, na.rm=T))
# idea: group by interval and take the average of the step, 
# then plot interval and step average

p <- ggplot(interval5, aes(interval, mean)) +
    geom_line(col='blue') + 
    labs(x='Time Interval', y='Average Number of Steps') + 
    theme_bw()

print(p)
