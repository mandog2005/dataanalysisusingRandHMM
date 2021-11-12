#how the system should work and you can check anomaly on the system itself
#What is the point? : trying to learn a normal behaviour of the system.
library(zoo)
library(lubridate)
library(ggplot2)
library(TTR)
library(tidyverse)
library(dplyr)

getwd()
setwd("C:/Users/alial/OneDrive/Desktop/CMPT318/Group Assignments/Assignment 3")
df <- read.table("Group_Assignment_3_Dataset.txt" & hour(df$Time) > 7 & hour(df$Time) < 10, header = TRUE, sep = " & hour(df$Time) > 7 & hour(df$Time) < 10,")
dates <- dmy(df$Date)
df$Date<- as.Date(dmy(df$Date))
remove(dates)
df$Time <- df$Time <- as_datetime(df$Date + hms(df$Time))

dfAll <- bind_rows(df1, df2)

df1 <- df[df$Date >= "2007-01-01" & df$Date <= "2007-01-07" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df2 <- df[df$Date >= "2007-01-08" & df$Date <= "2007-01-08" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df3 <- df[df$Date >= "2007-01-15" & df$Date <= "2007-01-15" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df4 <- df[df$Date >= "2007-01-22" & df$Date <= "2007-01-22" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df5 <- df[df$Date >= "2007-01-29" & df$Date <= "2007-01-29" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df6 <- df[df$Date >= "2007-02-05" & df$Date <= "2007-02-05" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df7 <- df[df$Date >= "2007-02-10" & df$Date <= "2007-02-10" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df8 <- df[df$Date >= "2007-02-17" & df$Date <= "2007-02-17" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df9 <- df[df$Date >= "2007-02-24" & df$Date <= "2007-02-24" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df10 <- df[df$Date >= "2007-03-02" & df$Date <= "2007-03-02" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df11 <- df[df$Date >= "2007-03-09" & df$Date <= "2007-03-09" & hour(df$Time) > 7 & hour(df$Time) < 10,]

df11 <- df[df$Date >= "2007-03-16" & df$Date <= "2007-03-22" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df12 <- df[df$Date >= "2007-03-22" & df$Date <= "2007-03-28" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df13 <- df[df$Date >= "2007-03-29" & df$Date <= "2007-04-04" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df14 <- df[df$Date >= "2007-04-05" & df$Date <= "2007-04-11" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df15 <- df[df$Date >= "2007-04-12" & df$Date <= "2007-04-18" & hour(df$Time) > 7 & hour(df$Time) < 10,]


# df16 to df20 ---- 

df16 <- df[df$Date >= "2007-04-19" & df$Date <= "2007-04-25" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df17 <- df[df$Date >= "2007-04-26" & df$Date <= "2007-05-02" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df18 <- df[df$Date >= "2007-05-03" & df$Date <= "2007-05-09" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df19 <- df[df$Date >= "2007-05-10" & df$Date <= "2007-05-16" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df20 <- df[df$Date >= "2007-05-17" & df$Date <= "2007-05-23" & hour(df$Time) > 7 & hour(df$Time) < 10,]


# df21 to df25 ------

df21 <- df[df$Date >= "2007-05-24" & df$Date <= "2007-05-30" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df22 <- df[df$Date >= "2007-05-31" & df$Date <= "2007-06-06" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df23 <- df[df$Date >= "2007-06-07" & df$Date <= "2007-06-13" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df24 <- df[df$Date >= "2007-06-14" & df$Date <= "2007-06-20" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df25 <- df[df$Date >= "2007-06-21" & df$Date <= "2007-06-27" & hour(df$Time) > 7 & hour(df$Time) < 10,]


# df26 to df30 -----

df26 <- df[df$Date >= "2007-06-28" & df$Date <= "2007-07-04" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df27 <- df[df$Date >= "2007-07-05" & df$Date <= "2007-07-11" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df28 <- df[df$Date >= "2007-07-12" & df$Date <= "2007-07-18" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df29 <- df[df$Date >= "2007-07-19" & df$Date <= "2007-07-25" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df30 <- df[df$Date >= "2007-07-26" & df$Date <= "2007-08-01" & hour(df$Time) > 7 & hour(df$Time) < 10,]



# df31 to df35 -----

df31 <- df[df$Date >= "2007-08-02" & df$Date <= "2007-08-08" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df32 <- df[df$Date >= "2007-08-09" & df$Date <= "2007-08-15" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df33 <- df[df$Date >= "2007-08-16" & df$Date <= "2007-08-22" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df34 <- df[df$Date >= "2007-08-23" & df$Date <= "2007-08-29" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df35 <- df[df$Date >= "2007-08-30" & df$Date <= "2007-09-05" & hour(df$Time) > 7 & hour(df$Time) < 10,]



# df36 to df40 ----

df36 <- df[df$Date >= "2007-09-06" & df$Date <= "2007-09-12" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df37 <- df[df$Date >= "2007-09-13" & df$Date <= "2007-09-19" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df38 <- df[df$Date >= "2007-09-20" & df$Date <= "2007-09-26" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df39 <- df[df$Date >= "2007-09-27" & df$Date <= "2007-10-03" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df40 <- df[df$Date >= "2007-10-04" & df$Date <= "2007-10-10" & hour(df$Time) > 7 & hour(df$Time) < 10,]


# df41 to df45 ------

df41 <- df[df$Date >= "2007-10-11" & df$Date <= "2007-10-17" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df42 <- df[df$Date >= "2007-10-18" & df$Date <= "2007-10-24" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df43 <- df[df$Date >= "2007-10-25" & df$Date <= "2007-10-31" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df44 <- df[df$Date >= "2007-11-01" & df$Date <= "2007-11-07" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df45 <- df[df$Date >= "2007-11-08" & df$Date <= "2007-11-14" & hour(df$Time) > 7 & hour(df$Time) < 10,]


# df46 to df49 -----

df46 <- df[df$Date >= "2007-11-15" & df$Date <= "2007-11-21" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df47 <- df[df$Date >= "2007-11-22" & df$Date <= "2007-11-28" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df48 <- df[df$Date >= "2007-11-29" & df$Date <= "2007-12-05" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df49 <- df[df$Date >= "2007-12-06" & df$Date <= "2007-12-12" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df50 <- df[df$Date >= "2007-12-13" & df$Date <= "2007-12-19" & hour(df$Time) > 7 & hour(df$Time) < 10,]

# df50 to df52 ----


df51 <- df[df$Date >= "2007-12-20" & df$Date <= "2007-12-26" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df52 <- df[df$Date >= "2007-12-27" & df$Date <= "2007-12-31" & hour(df$Time) > 7 & hour(df$Time) < 10,]


