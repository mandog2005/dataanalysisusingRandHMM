#how the system should work and you can check anomaly on the system itself
#What is the point? : trying to learn a normal behaviour of the system.
library(zoo)
library(lubridate)
library(ggplot2)
library(TTR)
library(tidyverse)
library(dplyr)
library(depmixS4)
library(ggfortify)
library(missMDA)

getwd()
setwd("C:/Users/alial/OneDrive/Desktop/CMPT318/Group Assignments/Term Project")
df <- read.table("TermProjectData.txt" , header = TRUE, sep = ",")
set.seed(1)
dates <- dmy(df$Date)
df$Date<- as.Date(dmy(df$Date))
remove(dates)
df$Time <- df$Time <- as_datetime(df$Date + hms(df$Time))



#this is the PCA analysis part
#saves data for principal analysis chart.

#ANALYSIS OF ALL DATA FROM 4 YEARS
myPrALL <- prcomp(na.omit(df[,3:9]), scale = TRUE)
summary(myPrALL)
myPrALL
head(myPrALL)

#ANALYSIS OF DATA FROM 2007 TO 2009 (2 YEARS)
df1 <- df[df$Date >= "2007-01-01" & df$Date <= "2009-01-01",]
myPr1 <- prcomp(na.omit(df1[,3:9]), scale = TRUE)
summary(myPr1)
myPrALL
head(myPr1)

#ANALYSIS OF DATA FROM JAN 2010 TO JULY 2010 (6 MONTHS)
df2 <- df[df$Date >= "2009-01-01" & df$Date <= "2009-06-01",]
myPr2 <- prcomp(na.omit(df2[,3:9]), scale = TRUE)
summary(myPr1)
myPrALL
head(myPr1)



#converting my data to CSV for future reference and all that good good.
dfRotationALL <- data.frame(myPrALL$rotation)
dfRotation1 <- data.frame(myPr1$rotation)
view(dfRotation1)
write.csv(dfRotation1,"rotation1.csv", row.names = TRUE)

#saving rotation2.
dfRotation2 <- data.frame(myPr2$rotation)
view(dfRotation2)
write.csv(dfRotation2,"rotation2.csv", row.names = TRUE)

pca.var.ALL <- myPrALL$sdev^2
pca.var.per.ALL <- round(pca.var.ALL/sum(pca.var.ALL)*100, 1)


barplot(pca.var.per.ALL ,names.arg=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7"))
#autoplot(myPrALL) data too big.

#######################################################
#HMM AND TESTING


####### THIS FUNCTION CUTS THE DATES OF THE DATA. #############

weeks_df = df %>% 
  mutate(week = cut.Date(Date, breaks = "1 week", labels = FALSE))

list_of_dfs <- split(weeks_df, weeks_df$week)
list2env(list_of_dfs, envir=.GlobalEnv)

################################################################


#you can use list of dfs for reference over here.



dfDaily1 <- df[df$Date >= "2006-12-18" & df$Date <= "2006-12-23"  & hour(df$Time) > 5 & hour(df$Time) < 11,]
dfDaily2 <- df[df$Date >= "2006-12-24" & df$Date <= "2006-12-29"  & hour(df$Time) > 5 & hour(df$Time) < 11,]
dfDaily3 <- df[df$Date >= "2006-12-18" & df$Date <= "2006-12-23"  & hour(df$Time) > 5 & hour(df$Time) < 11,]
dfDaily4 <- df[df$Date >= "2006-12-24" & df$Date <= "2006-12-29"  & hour(df$Time) > 5 & hour(df$Time) < 11,]
dfDaily5 <- df[df$Date >= "2006-12-18" & df$Date <= "2006-12-23"  & hour(df$Time) > 5 & hour(df$Time) < 11,]



week.intensity1 = zoo(dfDaily1$Global_active_power)
week.intensity2 = zoo(dfDaily2$Global_active_power)

sma1 = rollmean(week.intensity1,10)
sma2 = rollmean(week.intensity2,10)

autoplot(sma2)

mod1 <- depmix(Global_active_power~1, data = dfDaily1, nstates = 23)
mod1 <- depmix(list(dfDaily1$Global_active_power~1,dfDaily1$Global_intensity~1), data = iris, nstates = 2,family=list(gaussian(),multinomial("identity"), ntimes = c(1800)))
fm1 <- fit(mod1)

summary(fm1)
print(fm1)
