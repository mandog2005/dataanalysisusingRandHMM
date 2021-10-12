#how the system should work and you can check anomaly on the system itself
#What is the point? : trying to learn a normal behaviour of the system.

library(lubridate)

getwd()
setwd("C:/Users/alial/OneDrive/Desktop/318_ASSIGNMENTS_REPOSITORY/CMPT318_FALL2021/Assignment 1")
df <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")
dates <- dmy(df$Date)
df$Date<- as.Date(dates)

df <- df[df$Date >= "2007-03-05" & df$Date <= "2007-03-11",]
remove(dates)
library(ggplot2)
# function to get the mode.
getmode <- function(v, na.rm = FALSE) {
  
  if(na.rm){
    v = v[!is.na(v)]
  }
  
  uniqv <- unique(v)
  return (uniqv[which.max(tabulate(match(v, uniqv)))])
}

#TASK 1 start

#Data Exploration
#The goal of this assignment is data exploration. The purpose of the data exploration phase is
#getting a better understanding of the basic data characteristics. Besides the quality of the
#data, like completeness, validity, accuracy, consistency, availability and timeliness, this also
#includes aspects such as trends, seasonality, feature correlation and more. Technically, the
#electricity consumption data considered here represents a multivariate time series1 describing
#the power consumption behaviour observed over time, one datapoint per minute. The timedependent
#variables (also called response) are the following ones:
 

# A. Global_active_power
#B. Global_reactive_power
#C. Voltage
#D. Global_intensity
#E. Submetering 1
#F. Submetering 2
#G. Submetering 3



####################################################

#TASK 1 part A week 10:

#function for computing Arithmetic mean for part A week 10 week 10
avgActivePowerArithMeanA <- mean(df$Global_active_power, na.rm = TRUE)
print(avgActivePowerArithMeanA)

#function for computing Geometric mean for part A week 10 week 10
avgActivePowerGeometricMeanA <- exp(mean(log(df$Global_active_power), na.rm=TRUE))
print(avgActivePowerGeometricMeanA)


#function for computing median for part A week 10
#na.rm = TRUE just means we ignore null values in this case.
avgActivePowerMedianA <- median(df$Global_active_power , na.rm = TRUE)
print(avgActivePowerMedianA)


#function for computing mode for part A week 10
avgActivePowerModeA <- getmode(df$Global_active_power)
print(avgActivePowerModeA)

#function for computing std for part A week 10
StdActivePowerA <- sd(df$Global_active_power , na.rm = TRUE)
print(StdActivePowerA)

#Task A done


#TASK 1 part B week 10:


####################################################

#TASK 1 part B week 10:

#function for computing Arithmetic mean for part B week 10
avgReactivePowerArithMeanB <- mean(df$Global_reactive_power , na.rm = TRUE)
print(avgReactivePowerArithMeanB)

#function for computing Geometric mean for part B week 10
avgReactivePowerGeometricMeanB <- exp(mean(log(df$Global_reactive_power)& df$Date >= "2007-03-05" & df$Date <= "2007-03-11",na.rm = TRUE))
print(avgReactivePowerGeometricMeanB)

#function for computing median for part B week 10
avgReactivePowerMedianB <- median(df$Global_reactive_power& df$Date >= "2007-03-05" & df$Date <= "2007-03-11", na.rm = TRUE)
print(avgReactivePowerMedianB)

#function for computing mode for part B week 10
avgReactivePowerModeB <- getmode(df$Global_reactive_power,na.rm = TRUE)
print(avgReactivePowerModeB)

#function for computing std for part B week 10
StdReactivePowerB <- sd(df$Global_reactive_power& df$Date >= "2007-03-05" & df$Date <= "2007-03-11",na.rm = TRUE)
print(StdReactivePowerB)


#task B done


####################################################

#TASK 1 part C week 10 week 10:


#function for computing Arithmetic mean for part C week 10
avgVoltageArithMeanC <- mean(df$Voltage, na.rm = TRUE)
print(avgVoltageArithMeanC)#checking value

#function for computing Geometric mean for part C week 10
avgVoltageGeoMeanC <- exp(mean(log(df$Voltage) ,na.rm = TRUE))
print(avgVoltageGeoMeanC) #checking value

#function for computing median for part C week 10
avgVoltageMedianC <- median(df$Voltage& df$Date >= "2007-03-05" & df$Date <= "2007-03-11",na.rm = TRUE)
print(avgVoltageMedianC) #checking value

#function for computing mode for part C week 10
avgVoltageModeC <- getmode(df$Voltage& df$Date >= "2007-03-05" & df$Date <= "2007-03-11", na.rm = TRUE)

#function for computing std for part C week 10
stdVoltageC <- sd(df$Voltage& df$Date >= "2007-03-05" & df$Date <= "2007-03-11",na.rm = TRUE)
print(stdVoltageC)

#Task C Done
#TASK 1 part C week 10:


#######################################################

#TASK 1 PART 4 COMPUTE MIN OF A AND B:
#df[,1] == df(df$Date)

#max value for active power between monday to friday
maxValueWeekdayActive <- max(df[,3],na.rm = TRUE, wday(df[,1]) > 0 & wday(df[,1]) < 6)
#min value for active power between monday to friday
minValueWeekdayActive <- min(df[,3],na.rm = TRUE, wday(df[,1]) > 0 & wday(df[,1]) < 6)

#max value for Reactive power between monday to friday
maxValueWeekdayReactive <- max(df[,4],na.rm = TRUE, wday(df[,1]) > 0 & wday(df[,1]) < 6)
#min value for Reactive power between monday to friday
minValueWeekdayReactive <- min(df[,4],na.rm = TRUE, wday(df[,1]) > 0 & wday(df[,1]) < 6)


#max value for active power between saturday to sunday between morning and night
#maxValueWeekendActiveMorning <- max(df[,3], wday(df[,1]) > 5 & wday(df[,1]) < 8 & hour(timeHour) > 0 & hour(timeHour) < 12, na.rm = TRUE)
#min value for active power between monday to friday



#VOLTAGE
#max value for Voltage between monday to friday
#maxValueWeekdayVoltage <- max(df[,5],na.rm = TRUE, wday(df[,1]) > 0 & wday(df[,1]) < 6)
#min value for Voltage between monday to friday
#minValueWeekdayVoltage <- min(df[,5],na.rm = TRUE, wday(df[,1]) > 0 & wday(df[,1]) < 6)


#task 1 done

################################################################
#task 2 start

# A. Global_active_power
#B. Global_reactive_power
#C. Voltage
#D. Global_intensity
#E. Submetering 1
#F. Submetering 2
#G. Submetering 3



#correlation between A-D with submetering 1-3

#ABOUT DATA
#Date: Date in format dd/mm/yyyy
#Time: time in format hh:mm:ss

#A #Global_active_power: household global minute-averaged active power (in kilowatt).Global_reactive_power: household global minute-averaged reactive power (in kilowatt).
#B #Voltage: minute-averaged voltage (in volt)
#C #Global_intensity: household global minute-averaged current intensity (in ampere)
#E #Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). 
#~ #It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered) 
#F #Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy).
#~ #It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light. 
#G #Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). 
#~ #It corresponds to an electric water-heater and an air-conditioner.


#Correlation of Reactive to Active Power
CorrAB <- cor(df$Global_active_power,df$Global_reactive_power,use="complete.obs",method="pearson")
#Correlation of Reactive to 
CorrAB <- cor(df$Global_active_power,df$Global_reactive_power,use="complete.obs",method="pearson")
#Correlation of Reactive to Active Power
CorrAB <- cor(df$Global_active_power,df$Global_reactive_power,use="complete.obs",method="pearson")


#active power in kilowatt given that kitchen is used
CorrAE <- cor(df$Global_active_power,df$Sub_metering_1,use="complete.obs",method="pearson")
CorrBE <- cor(df$Global_reactive_power,df$Sub_metering_1,use="complete.obs",method="pearson")
CorrCE <- cor(df$Voltage,df$Sub_metering_1,use="complete.obs",method="pearson")
CorrDE <- cor(df$Global_intensity,df$Sub_metering_1,use="complete.obs",method="pearson")

CorrAF <- cor(df$Global_active_power,df$Sub_metering_2,use="complete.obs",method="pearson")
CorrBF <- cor(df$Global_reactive_power,df$Sub_metering_2,use="complete.obs",method="pearson")
CorrCF <- cor(df$Voltage,df$Sub_metering_2,use="complete.obs",method="pearson")
CorrDF <- cor(df$Global_intensity,df$Sub_metering_2,use="complete.obs",method="pearson")

CorrAG <- cor(df$Global_active_power,df$Sub_metering_3,use="complete.obs",method="pearson")
CorrBG <- cor(df$Global_reactive_power,df$Sub_metering_3,use="complete.obs",method="pearson")
CorrCG <- cor(df$Voltage,df$Sub_metering_3,use="complete.obs",method="pearson")
CorrDG <- cor(df$Global_intensity,df$Sub_metering_3,use="complete.obs",method="pearson")


CorrEF <- cor(df$Sub_metering_1,df$Sub_metering_2,use="complete.obs",method="pearson")
CorrEG <- cor(df$Sub_metering_1,df$Sub_metering_3,use="complete.obs",method="pearson")
CorrFG <- cor(df$Sub_metering_2,df$Sub_metering_3,use="complete.obs",method="pearson")


ggplot()+
  layer(data = df, mapping = aes(x=Time, y=Global_active_power), geom = "point",stat="identity", position = position_identity())

ggplot()+
  layer(data = df, mapping = aes(x=Time, y=Voltage), geom = "point",stat="identity", position = position_identity())

