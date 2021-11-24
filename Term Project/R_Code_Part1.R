data("CO2")
d <- CO2
head(d)
d

#####################

getwd()
setwd("C:/Users/aysha/Desktop/CMPT318(Fall2021)")
df <- read.csv("Trajectory.csv", header = TRUE)
class(df)
head(df)

df[10,3]
df[10,]
df[,3]


off <- df[df$STATUS == "OFF",]
on <- df[df$STATUS == "ON",]

class(off)
head(off)
print(off$STATUS)

print(off$SPEED) # Speed of the car is zero when its off!!! 


avgSpeed <- mean(df$SPEED)
print(avgSpeed)

overSpeedNight <- df[df$SPEED > 75 & df$DAY.NIGHT == "Night",]
nrow(overSpeedNight)

overSpeedDay <- df[df$SPEED > 75 & df$DAY.NIGHT == "Day",]
nrow(overSpeedDay)

print(mean(df$SPEED[df$DAY.NIGHT == "Night"]))
print(mean(df$SPEED[df$DAY.NIGHT == "Day"]))
