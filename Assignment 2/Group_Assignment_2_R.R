#how the system should work and you can check anomaly on the system itself
#What is the point? : trying to learn a normal behaviour of the system.
library(zoo)
library(lubridate)
library(ggplot2)
library(TTR)
library(tidyverse)
library(dplyr)

getwd()
setwd("C:/Users/alial/OneDrive/Desktop/CMPT318/Group Assignments/Assignment 2")
df <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")
dates <- dmy(df$Date)
df$Date<- as.Date(dmy(df$Date))
remove(dates)
df$Time <- df$Time <- as_datetime(df$Date + hms(df$Time))


#ggplot(df1, aes(x = Time, y = Global_intensity)) + geom_line()



# df1 to df15 ------

df1 <- df[df$Date >= "2008-01-01" & df$Date <= "2008-01-07",]
df2 <- df[df$Date >= "2008-01-08" & df$Date <= "2008-01-14",]
df3 <- df[df$Date >= "2008-01-15" & df$Date <= "2008-01-21",]
df4 <- df[df$Date >= "2008-01-22" & df$Date <= "2008-01-28",]
df5A <- df[df$Date >= "2008-01-29" & df$Date <= "2008-02-04",]
df5B <- df[df$Date >= "2008-02-05" & df$Date <= "2008-02-11",]
df6 <- df[df$Date >= "2008-02-10" & df$Date <= "2008-02-16",]
df7 <- df[df$Date >= "2008-02-17" & df$Date <= "2008-02-23",]
df8 <- df[df$Date >= "2008-02-24" & df$Date <= "2008-03-01",]
df9 <- df[df$Date >= "2008-03-02" & df$Date <= "2008-03-08",]
df10 <- df[df$Date >= "2008-03-09" & df$Date <= "2008-03-15",]

df11 <- df[df$Date >= "2008-03-16" & df$Date <= "2008-03-22",]
df12 <- df[df$Date >= "2008-03-22" & df$Date <= "2008-03-28",]
df13 <- df[df$Date >= "2008-03-29" & df$Date <= "2008-04-04",]
df14 <- df[df$Date >= "2008-04-05" & df$Date <= "2008-04-11",]
df15 <- df[df$Date >= "2008-04-12" & df$Date <= "2008-04-18",]


# df16 to df20 ---- 

df16 <- df[df$Date >= "2008-04-19" & df$Date <= "2008-04-25",]
df17 <- df[df$Date >= "2008-04-26" & df$Date <= "2008-05-02",]
df18 <- df[df$Date >= "2008-05-03" & df$Date <= "2008-05-09",]
df19 <- df[df$Date >= "2008-05-10" & df$Date <= "2008-05-16",]
df20 <- df[df$Date >= "2008-05-17" & df$Date <= "2008-05-23",]


# df21 to df25 ------

df21 <- df[df$Date >= "2008-05-24" & df$Date <= "2008-05-30",]
df22 <- df[df$Date >= "2008-05-31" & df$Date <= "2008-06-06",]
df23 <- df[df$Date >= "2008-06-07" & df$Date <= "2008-06-13",]
df24 <- df[df$Date >= "2008-06-14" & df$Date <= "2008-06-20",]
df25 <- df[df$Date >= "2008-06-21" & df$Date <= "2008-06-27",]


# df26 to df30 -----

df26 <- df[df$Date >= "2008-06-28" & df$Date <= "2008-07-04",]
df27 <- df[df$Date >= "2008-07-05" & df$Date <= "2008-07-11",]
df28 <- df[df$Date >= "2008-07-12" & df$Date <= "2008-07-18",]
df29 <- df[df$Date >= "2008-07-19" & df$Date <= "2008-07-25",]
df30 <- df[df$Date >= "2008-07-26" & df$Date <= "2008-08-01",]



# df31 to df35 -----

df31 <- df[df$Date >= "2008-08-02" & df$Date <= "2008-08-08",]
df32 <- df[df$Date >= "2008-08-09" & df$Date <= "2008-08-15",]
df33 <- df[df$Date >= "2008-08-16" & df$Date <= "2008-08-22",]
df34 <- df[df$Date >= "2008-08-23" & df$Date <= "2008-08-29",]
df35 <- df[df$Date >= "2008-08-30" & df$Date <= "2008-09-05",]



# df36 to df40 ----

df36 <- df[df$Date >= "2008-09-06" & df$Date <= "2008-09-12",]
df37 <- df[df$Date >= "2008-09-13" & df$Date <= "2008-09-19",]
df38 <- df[df$Date >= "2008-09-20" & df$Date <= "2008-09-26",]
df39 <- df[df$Date >= "2008-09-27" & df$Date <= "2008-10-03",]
df40 <- df[df$Date >= "2008-10-04" & df$Date <= "2008-10-10",]


# df41 to df45 ------

df41 <- df[df$Date >= "2008-10-11" & df$Date <= "2008-10-17",]
df42 <- df[df$Date >= "2008-10-18" & df$Date <= "2008-10-24",]
df43 <- df[df$Date >= "2008-10-25" & df$Date <= "2008-10-31",]
df44 <- df[df$Date >= "2008-11-01" & df$Date <= "2008-11-07",]
df45 <- df[df$Date >= "2008-11-08" & df$Date <= "2008-11-14",]


# df46 to df49 -----

df46 <- df[df$Date >= "2008-11-15" & df$Date <= "2008-11-21",]
df47 <- df[df$Date >= "2008-11-22" & df$Date <= "2008-11-28",]
df48 <- df[df$Date >= "2008-11-29" & df$Date <= "2008-12-05",]
df49 <- df[df$Date >= "2008-12-06" & df$Date <= "2008-12-12",]
df50 <- df[df$Date >= "2008-12-13" & df$Date <= "2008-12-19",]

# df50 to df52 ----


df51 <- df[df$Date >= "2008-12-20" & df$Date <= "2008-12-26",]
df52 <- df[df$Date >= "2008-12-27" & df$Date <= "2008-12-31",]



#week 1-5 ------
allweek.intensity = zoo(df$Global_intensity, 
                        order.by = seq(as.POSIXct("2008-01-01 00:00:00"), length=527040, by="min"))

week1.intensity = zoo(df1$Global_intensity, 
             order.by = seq(as.POSIXct("2008-01-01 00:00:00"), length=10080, by="min"))

week2.intensity = zoo(df2$Global_intensity, 
             order.by = seq(as.POSIXct("2008-01-08 00:00:00"), length=10080, by="min"))

week3.intensity = zoo(df3$Global_intensity, 
             order.by = seq(as.POSIXct("2008-01-15 00:00:00"), length=10080, by="min"))

week4.intensity = zoo(df4$Global_intensity, 
             order.by = seq(as.POSIXct("2008-01-22 00:00:00"), length=10080, by="min"))

week5A.intensity = zoo(df5A$Global_intensity, 
             order.by = seq(as.POSIXct("2008-01-29 00:00:00"), length=10080, by="min"))

week5B.intensity = zoo(df5B$Global_intensity, 
             order.by = seq(as.POSIXct("2008-02-05 00:00:00"), length=10080, by="min"))

#week 6-10 ---------
week6.intensity = zoo(df6$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-02-10 00:00:00"), length=10080, by="min"))

week7.intensity = zoo(df7$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-02-17 00:00:00"), length=10080, by="min"))


week8.intensity = zoo(df8$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-02-24 00:00:00"), length=10080, by="min"))


week9.intensity = zoo(df9$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-03-02 00:00:00"), length=10080, by="min"))


week10.intensity = zoo(df10$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-03-09 00:00:00"), length=10080, by="min"))

#week 11-15 ---------

week11.intensity = zoo(df11$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-03-16 00:00:00"), length=10080, by="min"))

week12.intensity = zoo(df12$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-03-22 00:00:00"), length=10080, by="min"))


week13.intensity = zoo(df13$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-03-29 00:00:00"), length=10080, by="min"))


week14.intensity = zoo(df14$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-04-05 00:00:00"), length=10080, by="min"))


week15.intensity = zoo(df15$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-04-12 00:00:00"), length=10080, by="min"))

#week 16-20 --------
week16.intensity = zoo(df16$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-04-19 00:00:00"), length=10080, by="min"))

week17.intensity = zoo(df17$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-04-26 00:00:00"), length=10080, by="min"))


week18.intensity = zoo(df18$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-05-03 00:00:00"), length=10080, by="min"))


week19.intensity = zoo(df19$Global_intensity, 
                     order.by = seq(as.POSIXct("2008-05-10 00:00:00"), length=10080, by="min"))


week20.intensity = zoo(df20$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-05-17 00:00:00"), length=10080, by="min"))
#week 21-25 ----

week21.intensity = zoo(df21$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-05-24 00:00:00"), length=10080, by="min"))

week22.intensity = zoo(df22$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-05-31 00:00:00"), length=10080, by="min"))


week23.intensity = zoo(df23$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-06-07 00:00:00"), length=10080, by="min"))


week24.intensity = zoo(df24$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-06-14 00:00:00"), length=10080, by="min"))


week25.intensity = zoo(df25$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-06-21 00:00:00"), length=10080, by="min"))

#week 26-30 -----
week26.intensity = zoo(df26$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-06-28 00:00:00"), length=10080, by="min"))

week27.intensity = zoo(df27$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-07-05 00:00:00"), length=10080, by="min"))


week28.intensity = zoo(df28$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-07-12 00:00:00"), length=10080, by="min"))


week29.intensity = zoo(df29$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-07-19 00:00:00"), length=10080, by="min"))


week30.intensity = zoo(df30$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-07-26 00:00:00"), length=10080, by="min"))

#week 31-35 -----
week31.intensity = zoo(df31$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-08-02 00:00:00"), length=10080, by="min"))

week32.intensity = zoo(df32$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-08-09 00:00:00"), length=10080, by="min"))


week33.intensity = zoo(df33$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-08-16 00:00:00"), length=10080, by="min"))


week34.intensity = zoo(df34$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-08-23 00:00:00"), length=10080, by="min"))


week35.intensity = zoo(df35$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-08-30 00:00:00"), length=10080, by="min"))

#week 36-40 -----
week36.intensity = zoo(df36$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-09-06 00:00:00"), length=10080, by="min"))

week37.intensity = zoo(df37$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-09-13 00:00:00"), length=10080, by="min"))


week38.intensity = zoo(df38$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-09-20 00:00:00"), length=10080, by="min"))


week39.intensity = zoo(df39$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-09-27 00:00:00"), length=10080, by="min"))


week40.intensity = zoo(df40$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-10-04 00:00:00"), length=10080, by="min"))

#week 41-45 -----
week41.intensity = zoo(df41$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-10-11 00:00:00"), length=10080, by="min"))

week42.intensity = zoo(df42$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-10-18 00:00:00"), length=10080, by="min"))


week43.intensity = zoo(df43$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-10-25 00:00:00"), length=10080, by="min"))


week44.intensity = zoo(df44$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-11-01 00:00:00"), length=10080, by="min"))


week45.intensity = zoo(df45$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-11-08 00:00:00"), length=10080, by="min"))


#week 46-50 -----
week46.intensity = zoo(df46$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-11-15 00:00:00"), length=10080, by="min"))

week47.intensity = zoo(df47$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-11-22 00:00:00"), length=10080, by="min"))


week48.intensity = zoo(df48$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-11-29 00:00:00"), length=10080, by="min"))


week49.intensity = zoo(df49$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-12-06 00:00:00"), length=10080, by="min"))


week50.intensity = zoo(df50$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-12-13 00:00:00"), length=10080, by="min"))


#week 51-53 -----
week51.intensity = zoo(df51$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-12-20 00:00:00"), length=10080, by="min"))

week52.intensity = zoo(df52$Global_intensity, 
                      order.by = seq(as.POSIXct("2008-12-27 00:00:00"), length=10080, by="min"))


#----




smaAll = rollmean(allweek.intensity,180)

sma1 = rollmean(week1.intensity,10)
sma2 = rollmean(week2.intensity,10)
sma3 = rollmean(week3.intensity,10)
sma4 = rollmean(week4.intensity,10)
sma5 = rollmean(week5A.intensity,10)
sma5b = rollmean(week5B.intensity,10)

sma6 = rollmean(week6.intensity,10)
sma7 = rollmean(week7.intensity,10)
sma8 = rollmean(week8.intensity,10)
sma9 = rollmean(week9.intensity,10)
sma10 = rollmean(week10.intensity,10)

sma11 = rollmean(week11.intensity,10)
sma12 = rollmean(week12.intensity,10)
sma13 = rollmean(week13.intensity,10)
sma14 = rollmean(week14.intensity,10)
sma15 = rollmean(week15.intensity,10)

sma16 = rollmean(week16.intensity,10)
sma17 = rollmean(week17.intensity,10)
sma18 = rollmean(week18.intensity,10)
sma19 = rollmean(week19.intensity,10)
sma20 = rollmean(week20.intensity,10)

sma21 = rollmean(week21.intensity,10)
sma22 = rollmean(week22.intensity,10)
sma23 = rollmean(week23.intensity,10)
sma24 = rollmean(week24.intensity,10)
sma25 = rollmean(week25.intensity,10)

sma26 = rollmean(week26.intensity,10)
sma27 = rollmean(week27.intensity,10)
sma28 = rollmean(week28.intensity,10)
sma29 = rollmean(week29.intensity,10)
sma30 = rollmean(week30.intensity,10)

sma31 = rollmean(week31.intensity,10)
sma32 = rollmean(week32.intensity,10)
sma33 = rollmean(week33.intensity,10)
sma34 = rollmean(week34.intensity,10)
sma35 = rollmean(week35.intensity,10)

sma36 = rollmean(week36.intensity,10)
sma37 = rollmean(week37.intensity,10)
sma38 = rollmean(week38.intensity,10)
sma39 = rollmean(week39.intensity,10)
sma40 = rollmean(week40.intensity,10)

sma41 = rollmean(week41.intensity,10)
sma42 = rollmean(week42.intensity,10)
sma43 = rollmean(week43.intensity,10)
sma44 = rollmean(week44.intensity,10)
sma45 = rollmean(week45.intensity,10)

sma46 = rollmean(week46.intensity,10)
sma47 = rollmean(week47.intensity,10)
sma48 = rollmean(week48.intensity,10)
sma49 = rollmean(week49.intensity,10)
sma50 = rollmean(week50.intensity,10)

sma51 = rollmean(week51.intensity,10)
sma52 = rollmean(week52.intensity,10)




plot(sma7
     )


#smacompare = rollmean(allweek.intensity,10)
#lines(smacompare,col = "red")

dfsma1 <- as.data.frame(sma1)
dfsma2 <- as.data.frame(sma2)
dfsma3 <- as.data.frame(sma3)
dfsma4 <- as.data.frame(sma4)
dfsma5 <- as.data.frame(sma5)
dfsma6 <- as.data.frame(sma6)
dfsma7 <- as.data.frame(sma7)
dfsma8 <- as.data.frame(sma8)
dfsma9 <- as.data.frame(sma9)
dfsma10<- as.data.frame(sma10)
dfsma11 <- as.data.frame(sma11)
dfsma12 <- as.data.frame(sma12)
dfsma13 <- as.data.frame(sma13)
dfsma14 <- as.data.frame(sma14)
dfsma15 <- as.data.frame(sma15)
dfsma16 <- as.data.frame(sma16)
dfsma17 <- as.data.frame(sma17)
dfsma18 <- as.data.frame(sma18)
dfsma19 <- as.data.frame(sma19)
dfsma20 <- as.data.frame(sma20)
dfsma21 <- as.data.frame(sma21)
dfsma22 <- as.data.frame(sma22)
dfsma23 <- as.data.frame(sma23)
dfsma24 <- as.data.frame(sma24)
dfsma25 <- as.data.frame(sma25)

dfsma26 <- as.data.frame(sma26)
dfsma27 <- as.data.frame(sma27)
dfsma28 <- as.data.frame(sma28)
dfsma29 <- as.data.frame(sma29)
dfsma30 <- as.data.frame(sma30)
dfsma31 <- as.data.frame(sma31)
dfsma32 <- as.data.frame(sma32)
dfsma33 <- as.data.frame(sma33)
dfsma34 <- as.data.frame(sma34)
dfsma35<- as.data.frame(sma35)
dfsma36 <- as.data.frame(sma36)
dfsma37 <- as.data.frame(sma37)
dfsma38 <- as.data.frame(sma38)
dfsma39 <- as.data.frame(sma39)
dfsma40 <- as.data.frame(sma40)
dfsma41 <- as.data.frame(sma41)
dfsma42 <- as.data.frame(sma42)
dfsma43 <- as.data.frame(sma43)
dfsma44 <- as.data.frame(sma44)
dfsma45 <- as.data.frame(sma45)
dfsma46 <- as.data.frame(sma46)
dfsma47 <- as.data.frame(sma47)
dfsma48 <- as.data.frame(sma48)
dfsma49 <- as.data.frame(sma49)
dfsma50 <- as.data.frame(sma50)

dfsma51 <- as.data.frame(sma51)
dfsma52 <- as.data.frame(sma52)


