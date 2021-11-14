#how the system should work and you can check anomaly on the system itself
#What is the point? : trying to learn a normal behaviour of the system.
library(zoo)
library(lubridate)
library(ggplot2)
library(TTR)
library(tidyverse)
library(dplyr)
library(depmixS4)

getwd()
setwd("C:/Users/alial/OneDrive/Desktop/CMPT318/Group Assignments/Assignment 3")
df <- read.table("Group_Assignment_3_Dataset.txt" , header = TRUE, sep = ",")
set.seed(1)
dates <- dmy(df$Date)
df$Date<- as.Date(dmy(df$Date))
remove(dates)
df$Time <- df$Time <- as_datetime(df$Date + hms(df$Time))




ntimes1 <- read.csv("ntimes.csv" , header = TRUE, sep = ",")

#df1 to df16

df1 <- df[df$Date >= "2007-01-01" & df$Date <= "2007-01-01" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df2 <- df[df$Date >= "2007-01-08" & df$Date <= "2007-01-08" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df3 <- df[df$Date >= "2007-01-15" & df$Date <= "2007-01-15" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df4 <- df[df$Date >= "2007-01-22" & df$Date <= "2007-01-22" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df5 <- df[df$Date >= "2007-01-29" & df$Date <= "2007-01-29" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df6 <- df[df$Date >= "2007-02-05" & df$Date <= "2007-02-05" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df7 <- df[df$Date >= "2007-02-10" & df$Date <= "2007-02-10" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df8 <- df[df$Date >= "2007-02-10" & df$Date <= "2007-02-10" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df9 <- df[df$Date >= "2007-02-24" & df$Date <= "2007-02-24" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df10 <- df[df$Date >= "2007-03-02" & df$Date <= "2007-03-02" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df11 <- df[df$Date >= "2007-03-09" & df$Date <= "2007-03-09" & hour(df$Time) > 7 & hour(df$Time) < 10,]

df12 <- df[df$Date >= "2007-03-16" & df$Date <= "2007-03-16" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df13 <- df[df$Date >= "2007-03-22" & df$Date <= "2007-03-22" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df14 <- df[df$Date >= "2007-03-29" & df$Date <= "2007-03-29" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df15 <- df[df$Date >= "2007-04-05" & df$Date <= "2007-04-05" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df16 <- df[df$Date >= "2007-04-12" & df$Date <= "2007-04-12" & hour(df$Time) > 7 & hour(df$Time) < 10,]

# df16 to df20 ---- 

#df17 <- df[df$Date >= "2007-04-19" & df$Date <= "2007-04-19" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df18 <- df[df$Date >= "2007-04-26" & df$Date <= "2007-04-26" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df19 <- df[df$Date >= "2007-05-03" & df$Date <= "2007-05-03" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df20 <- df[df$Date >= "2007-05-10" & df$Date <= "2007-05-10" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df21 <- df[df$Date >= "2007-05-17" & df$Date <= "2007-05-17" & hour(df$Time) > 7 & hour(df$Time) < 10,]

# df21 to df25 ------

df22 <- df[df$Date >= "2007-05-24" & df$Date <= "2007-05-24" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df23 <- df[df$Date >= "2007-05-31" & df$Date <= "2007-05-31" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df24 <- df[df$Date >= "2007-06-07" & df$Date <= "2007-06-07" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df25 <- df[df$Date >= "2007-06-14" & df$Date <= "2007-06-14" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df26 <- df[df$Date >= "2007-06-21" & df$Date <= "2007-06-21" & hour(df$Time) > 7 & hour(df$Time) < 10,]

# df26 to df30 -----

df27 <- df[df$Date >= "2007-06-28" & df$Date <= "2007-06-28" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df28 <- df[df$Date >= "2007-07-05" & df$Date <= "2007-07-05" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df29 <- df[df$Date >= "2007-07-12" & df$Date <= "2007-07-12" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df30 <- df[df$Date >= "2007-07-19" & df$Date <= "2007-07-19" & hour(df$Time) > 7 & hour(df$Time) < 10,]



# df31 to df35 -----

#df31 <- df[df$Date >= "2007-07-26" & df$Date <= "2007-07-26" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df32 <- df[df$Date >= "2007-08-02" & df$Date <= "2007-08-02" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df33 <- df[df$Date >= "2007-08-09" & df$Date <= "2007-08-09" & hour(df$Time) > 7 & hour(df$Time) < 10,]
df34 <- df[df$Date >= "2007-08-16" & df$Date <= "2007-08-16" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df35 <- df[df$Date >= "2007-08-23" & df$Date <= "2007-08-23" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df36 <- df[df$Date >= "2007-08-30" & df$Date <= "2007-08-30" & hour(df$Time) > 7 & hour(df$Time) < 10,]



# df36 to df40 ----

#df37 <- df[df$Date >= "2007-09-06" & df$Date <= "2007-09-06" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df38 <- df[df$Date >= "2007-09-13" & df$Date <= "2007-09-13" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df39 <- df[df$Date >= "2007-09-20" & df$Date <= "2007-09-20" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df40 <- df[df$Date >= "2007-09-27" & df$Date <= "2007-09-27" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df41 <- df[df$Date >= "2007-10-04" & df$Date <= "2007-10-04" & hour(df$Time) > 7 & hour(df$Time) < 10,]


# df41 to df45 ------

#df42 <- df[df$Date >= "2007-10-11" & df$Date <= "2007-10-11" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df43 <- df[df$Date >= "2007-10-18" & df$Date <= "2007-10-18" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df44 <- df[df$Date >= "2007-10-25" & df$Date <= "2007-10-25" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df45 <- df[df$Date >= "2007-11-01" & df$Date <= "2007-11-01" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df46 <- df[df$Date >= "2007-11-08" & df$Date <= "2007-11-08" & hour(df$Time) > 7 & hour(df$Time) < 10,]


# df46 to df49 -----

#df47 <- df[df$Date >= "2007-11-15" & df$Date <= "2007-11-15" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df48 <- df[df$Date >= "2007-11-22" & df$Date <= "2007-11-22" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df49 <- df[df$Date >= "2007-11-29" & df$Date <= "2007-11-29" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df50 <- df[df$Date >= "2007-12-06" & df$Date <= "2007-12-06" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df51 <- df[df$Date >= "2007-12-13" & df$Date <= "2007-12-13" & hour(df$Time) > 7 & hour(df$Time) < 10,]


# df51 to df53 ----


#df52 <- df[df$Date >= "2007-12-20" & df$Date <= "2007-12-20" & hour(df$Time) > 7 & hour(df$Time) < 10,]
#df53 <- df[df$Date >= "2007-12-27" & df$Date <= "2007-12-27" & hour(df$Time) > 7 & hour(df$Time) < 10,]

#----

#binding fn, need to use this to concatenate all dataframes at the end.

dfAll <- bind_rows(df1, df2) 
dfAll <- bind_rows(dfAll, df3)
dfAll <- bind_rows(dfAll, df4)
dfAll <- bind_rows(dfAll, df5)
dfAll <- bind_rows(dfAll, df6)
dfAll <- bind_rows(dfAll, df7)

#dfAll <- bind_rows(dfAll, df8)

dfAll <- bind_rows(dfAll, df9)
dfAll <- bind_rows(dfAll, df10)
dfAll <- bind_rows(dfAll, df11)
dfAll <- bind_rows(dfAll, df12)
dfAll <- bind_rows(dfAll, df13)
dfAll <- bind_rows(dfAll, df14)
dfAll <- bind_rows(dfAll, df15)

dfAll <- bind_rows(dfAll, df16) 
#dfAll <- bind_rows(dfAll, df17) 
dfAll <- bind_rows(dfAll, df18)
dfAll <- bind_rows(dfAll, df19)
dfAll <- bind_rows(dfAll, df20)
dfAll <- bind_rows(dfAll, df21)
dfAll <- bind_rows(dfAll, df22)
dfAll <- bind_rows(dfAll, df23)
dfAll <- bind_rows(dfAll, df24)
dfAll <- bind_rows(dfAll, df25)
dfAll <- bind_rows(dfAll, df26)
dfAll <- bind_rows(dfAll, df27)
dfAll <- bind_rows(dfAll, df28)
dfAll <- bind_rows(dfAll, df29)
dfAll <- bind_rows(dfAll, df30)


#dfAll <- bind_rows(dfAll, df31) 
#dfAll <- bind_rows(dfAll, df32) 
dfAll <- bind_rows(dfAll, df33)
dfAll <- bind_rows(dfAll, df34)
#dfAll <- bind_rows(dfAll, df35)
#dfAll <- bind_rows(dfAll, df36)
#dfAll <- bind_rows(dfAll, df37)
#dfAll <- bind_rows(dfAll, df38)
#dfAll <- bind_rows(dfAll, df39)
#dfAll <- bind_rows(dfAll, df40)
#dfAll <- bind_rows(dfAll, df41)
#dfAll <- bind_rows(dfAll, df42)
#dfAll <- bind_rows(dfAll, df43)
#dfAll <- bind_rows(dfAll, df44)
#dfAll <- bind_rows(dfAll, df45)

#dfAll <- bind_rows(dfAll, df46) 
#dfAll <- bind_rows(dfAll, df47) 
#dfAll <- bind_rows(dfAll, df48)
#dfAll <- bind_rows(dfAll, df49)
#dfAll <- bind_rows(dfAll, df50)
#dfAll <- bind_rows(dfAll, df51)
#dfAll <- bind_rows(dfAll, df52)
#dfAll <- bind_rows(dfAll, df53)

dfAll$Global_active_power <- scale(dfAll$Global_active_power)
dfAll$Global_intensity <- scale(dfAll$Global_intensity)
dfAll$Global_reactive_power <- scale(dfAll$Global_reactive_power)
ntimes1 <- as.numeric(ntimes1$?..NTIMES)

model1 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =3, ntimes = ntimes1 )
fm1 <- fit(model1)


model2 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =4, ntimes = ntimes1 )
fm2 <- fit(model2)


model3 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =5, ntimes = ntimes1 )
fm3 <- fit(model3)


model4 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =6, ntimes = ntimes1 )
fm4 <- fit(model4)


model5 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =7, ntimes = ntimes1 )
fm5 <- fit(model5)

model6 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =8, ntimes = ntimes1 )
fm6 <- fit(model6)


model7 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =9, ntimes = ntimes1 )
fm7 <- fit(model7)

model8 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =10, ntimes = ntimes1 )
fm8 <- fit(model8)

model9 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =11, ntimes = ntimes1 )
fm9 <- fit(model4)


model10 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =12, ntimes = ntimes1 )
fm10 <- fit(modell0)

model11 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =13, ntimes = ntimes1 )
fm11 <- fit(model11)


model12 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =14, ntimes = ntimes1 )
fm12 <- fit(model12)

model13 <- depmix(response =dfAll$Global_intensity~1, data =dfAll, nstates =1, ntimes = ntimes1 )
fm13 <- fit(model13)



plot(1:13,c(BIC(fm1),BIC(fm2),BIC(fm3),BIC(fm4),BIC(fm5), BIC(fm6), BIC(fm7), BIC(fm8), BIC(fm9),BIC(fm10),BIC(fm11),BIC(fm12),BIC(fm13)),ty="b")


############ Mike's code ###################

model8 <- depmix(response =dfAll$Global_active_power~1, data =dfAll, nstates =16, ntimes = ntimes1 )
fm8 <- fit(model7)



model8 <- depmix(response =dfAll$Global_active_power~1, data =dfAll, nstates =13, ntimes = ntimes1 )
fm8 <- fit(model8)


model8 <- depmix(response =dfAll$Global_active_power~1, data =dfAll, nstates =12, ntimes = ntimes1 )
fm8 <- fit(model9)


model10 <- depmix(response =dfAll$Global_active_power~1, data =dfAll, nstates =7, ntimes = ntimes1 )
fm10 <- fit(model10)


model11 <- depmix(response =dfAll$Global_active_power~1, data =dfAll, nstates =4, ntimes = ntimes1 )
fm11 <- fit(model11)

model12 <- depmix(response =dfAll$Global_active_power~1, data =dfAll, nstates =2, ntimes = ntimes1 )
fm12 <- fit(model12)

plot(1:6,c(BIC(fm7),BIC(fm8),BIC(fm9),BIC(fm10),BIC(fm11), BIC(fm12)),ty="b")




