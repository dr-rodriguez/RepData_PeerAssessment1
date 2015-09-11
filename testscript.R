# Script to test out things before adding to the R Markdown file


activity <- read.csv('activity.csv', stringsAsFactors=T)
library(dplyr)

sumdata <- 
    activity %>%
    group_by(date) %>%
    summarize(sum=sum(steps, na.rm=T))
    

mean(sumdata$sum)
median(sumdata$sum)

library(ggplot2)

p <- ggplot(sumdata, aes(date, sum)) + 
    geom_bar(fill='darkgreen', col='white', stat="identity") +
    labs(x='Date', y='Total Number of Steps') + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle=90))

print(p)
#ggsave('figures/plot1.png')


