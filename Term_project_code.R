library(zoo)
library(lubridate)
library(ggplot2)
library(TTR)
library(tidyverse)
library(dplyr)
library(depmixS4)

getwd()
setwd("C:/Users/ahalden/Desktop/Term Project")
df <- read.table("TermProjectData.txt",header = TRUE,sep= ",")
dfNum <- subset(df, )

dates <- dmy(df$Date)
df$Date <- as.Date(dmy(df$Date))
remove(dates)

df$Time <- as_datetime(df$Date + hms(df$Time))

pca <- prcomp(t(df$Global_active_power), scale=TRUE, na.action = na.omit) 
