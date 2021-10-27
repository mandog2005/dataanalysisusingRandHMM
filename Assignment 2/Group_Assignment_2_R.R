#how the system should work and you can check anomaly on the system itself
#What is the point? : trying to learn a normal behaviour of the system.
library(lubridate)
library(tidyverse)

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

library(dplyr)
getwd()
setwd("C:/Users/alial/OneDrive/Desktop/318_ASSIGNMENTS_REPOSITORY/CMPT318_FALL2021/Assignment 2")
df <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")
dfMorning <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")
dates <- dmy(df$Date)
df$Date<- as.Date(dates)
df$Time <- as_datetime(df$Date + hms(df$Time))



#df <- df[df$Date >= "2007-03-05" & df$Date <= "2007-03-11",]
#remove(dates)
dfMorning <- df[hour(df$Time) >= 8 & hour(df$Time) <= 16,]
dfNight <- df[hour(df$Time) >= 17 & hour(df$Time) <= 23,]

