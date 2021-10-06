getwd()
df <- read.csv("C:/Users/alial/OneDrive/Desktop/CMPT318/Group Assignments/Assignment 1/Group_Assignment_1_Dataset.csv", header = TRUE)
library(lubridate)
# function to get the mode.
getmode <- function(v, na.rm = FALSE) {
  
  if(na.rm){
    v = v[!is.na(v)]
  }
  
  uniqv <- unique(v)
  return (uniqv[which.max(tabulate(match(v, uniqv)))])
}


####################################################

.#function for computing Arithmetic mean for part A
avgActivePowerArithMeanA <- mean(df$Global_active_power)
print(avgActivePowerArithMeanA)

#function for computing Geometric mean for part A
avgActivePowerGeometricMeanA <- exp(mean(log(df$Global_active_power)))
print(avgActivePowerGeometricMeanA)


#function for computing median for part A
avgActivePowerMedianA <- median(df$Global_active_power)
print(avgActivePowerMedianA)


#function for computing mode for part A
avgActivePowerModeA <- getmode(df$Global_active_power)

#function for computing std for part 1
StdActivePowerA <- sd(df$Global_active_power)
print(StdActivePowerA)

#Task A done


####################################################


#function for computing Arithmetic mean for part A
avgReactivePowerArithMeanB <- mean(df$Global_reactive_power, na.rm = TRUE)
print(avgReactivePowerArithMeanB)

#function for computing Geometric mean for part A
avgReactivePowerGeometricMeanB <- exp(mean(log(df$Global_reactive_power),na.rm = TRUE))
print(avgReactivePowerGeometricMeanB)

#function for computing median for part A
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




.#function for computing Arithmetic mean for part B
avgVoltageArithMeanC <- mean(df$Voltage,na.rm = TRUE)
print(avgVoltageArithMeanC)#checking value

#function for computing Geometric mean for part B
avgVoltageGeoMeanC <- exp(mean(log(df$Voltage),na.rm = TRUE))
print(avgVoltageGeoMeanC) #checking value

#function for computing median for part B
avgVoltageMedianC <- median(df$Voltage,na.rm = TRUE)
print(avgVoltageMedianC) #checking value

#function for computing mode for part B
avgVoltageModeC <- getmode(df$Voltage, na.rm = TRUE)

#function for computing std for part B
stdVoltageC <- sd(df$Voltage,na.rm = TRUE)
print(stdVoltageC)

#Task C Done
#end

#max value for active power
maxValueWeekdayActive <- max(df[,3],na.rm = TRUE & wday(df[,1]) > 0 & wday(df[,1]) < 6)
#min value for active power
minValueWeekdayActive <- min(df[,3],na.rm = TRUE & wday(df[,1]) > 0 & wday(df[,1]) < 6)

