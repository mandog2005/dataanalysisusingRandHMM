

getwd()
df <- read.table("C:/Users/alial/OneDrive/Desktop/CMPT318/Group Assignments/Assignment 1/Group_Assignment_1_Dataset.txt", sep = "," , header = TRUE)
#as.POSIXlt.Date(df[,1],format = "");

timeHour <- format(as.POSIXct(df$Time,format="%H:%M:%S"),"%H", header = TRUE) #hour
dfHours <- data.frame(timeHour)
dfHours <- as.numeric(as.character(dfHours$timeHour))
remove(timeHour)#waste of RAM


df <- read.csv("C:/Users/alial/OneDrive/Desktop/318_ASSIGNMENTS_REPOSITORY/CMPT318_FALL2021/Assignment 1/Group_Assignment_1_Dataset.csv", header = TRUE)

library(lubridate)
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

#TASK 1 PART A:

.#function for computing Arithmetic mean for part A
avgActivePowerArithMeanA <- mean(df$Global_active_power)
print(avgActivePowerArithMeanA)

#function for computing Geometric mean for part A
avgActivePowerGeometricMeanA <- exp(mean(log(df$Global_active_power)))
print(avgActivePowerGeometricMeanA)


#function for computing median for part A
#na.rm = TRUE just means we ignore null values in this case.
avgActivePowerMedianA <- median(df$Global_active_power, na.rm = TRUE)
print(avgActivePowerMedianA)


#function for computing mode for part A
avgActivePowerModeA <- getmode(df$Global_active_power, na.rm = TRUE)
print(avgActivePowerModeA)

#function for computing std for part A
StdActivePowerA <- sd(df$Global_active_power, na.rm = TRUE)
print(StdActivePowerA)

#Task A done


#TASK 1 PART B:


####################################################

#TASK 1 PART B:

#function for computing Arithmetic mean for part B
avgReactivePowerArithMeanB <- mean(df$Global_reactive_power, na.rm = TRUE)
print(avgReactivePowerArithMeanB)

#function for computing Geometric mean for part B
avgReactivePowerGeometricMeanB <- exp(mean(log(df$Global_reactive_power),na.rm = TRUE))
print(avgReactivePowerGeometricMeanB)

#function for computing median for part B
avgReactivePowerMedianB <- median(df$Global_reactive_power, na.rm = TRUE)
print(avgReactivePowerMedianB)

#function for computing mode for part B
avgReactivePowerModeB <- getmode(df$Global_reactive_power,na.rm = TRUE)
print(avgReactivePowerModeB)

#function for computing std for part B
StdReactivePowerB <- sd(df$Global_reactive_power,na.rm = TRUE)
print(StdReactivePowerB)


#task B done


####################################################

#TASK 1 PART c:


.#function for computing Arithmetic mean for part C
avgVoltageArithMeanC <- mean(df$Voltage,na.rm = TRUE)
print(avgVoltageArithMeanC)#checking value

#function for computing Geometric mean for part C
avgVoltageGeoMeanC <- exp(mean(log(df$Voltage),na.rm = TRUE))
print(avgVoltageGeoMeanC) #checking value

#function for computing median for part C
avgVoltageMedianC <- median(df$Voltage,na.rm = TRUE)
print(avgVoltageMedianC) #checking value

#function for computing mode for part C
avgVoltageModeC <- getmode(df$Voltage, na.rm = TRUE)

#function for computing std for part C
stdVoltageC <- sd(df$Voltage,na.rm = TRUE)
print(stdVoltageC)

#Task C Done
#TASK 1 PART c:


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


