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
library(imputeTS)

getwd()
setwd("C:/Users/alial/OneDrive/Desktop/CMPT318/Group Assignments/Term Project")
df <- read.table("TermProjectData.txt" , header = TRUE, sep = ",")
dates <- dmy(df$Date)
df$Date<- as.Date(dmy(df$Date))
remove(dates)
df$Time <- df$Time <- as_datetime(df$Date + hms(df$Time))

#fixes NA values using kalman approximations.
df[,3:9] <- na.kalman(df[,3:9])

#this is the PCA analysis part
#saves data for principal analysis chart.

#ANALYSIS OF ALL DATA FROM 4 YEARS
myPrALL <- prcomp(na.omit(df[,3:9]), scale = TRUE)
summary(myPrALL)
myPrALL
head(myPrALL)

#ANALYSIS OF DATA FROM 2007 TO 2009 (2 YEARS)
df1 <- df[df$Date == "2007-01-01",]
myPr1 <- prcomp(na.omit(df1[,3:9]), scale = TRUE)
summary(myPr1)
myPrALL
head(myPr1)

#ANALYSIS OF DATA FROM JAN 2010 TO JULY 2010 (6 MONTHS)
df2 <- df[df$Date >= "2009-01-01" & df$Date <= "2009-06-01",]
myPr2 <- prcomp(na.omit(df2[,3:9]), scale = TRUE)
summary(myPr2)
myPrALL
head(myPr2)

#ANALYSIS OF DATA FROM JAN 2010 TO JULY 2010 (1 MONTH)
df3 <- df[df$Date >= "2009-01-01" & df$Date <= "2009-02-01",]
myPr3 <- prcomp(na.omit(df3[,3:9]), scale = TRUE)
summary(myPr3)
myPr3
head(myPr3)
biplot(myPr3, scale = 0)


#EXTRACT PC SCORES
str(myPrALL)



dfPR1 <- cbind(df3, myPr3$x[,1:2])
view(dfPR1)

ggplot(dfPR1, aes(PC1,PC2, col = hour(Time), fill = hour(Time))) + 
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + geom_point(shape=21, col = "black")

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

autoplot(myPr2)

#######################################################
#HMM AND TESTING


####### THIS FUNCTION CUTS THE DATES OF THE DATA. #############

weeks_df = df %>% 
  mutate(week = cut.Date(Date, breaks = "1 week", labels = FALSE))

list_of_dfs <- split(weeks_df, weeks_df$week)
list2env(list_of_dfs, envir=.GlobalEnv)

################################################################


#you can use list of dfs for reference over here.

##### YEAR 1 ########


dfDaily1Y1 <- df[df$Date == "2006-12-18" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily2Y1 <- df[df$Date == "2006-12-25"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily3Y1 <- df[df$Date == "2007-01-01"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily4Y1 <- df[df$Date == "2007-01-08"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily5Y1 <- df[df$Date == "2007-01-15"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily6Y1 <- df[df$Date == "2007-01-22"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily7Y1 <- df[df$Date == "2007-01-29" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily8Y1 <- df[df$Date == "2007-02-05"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily9Y1 <- df[df$Date == "2007-02-12"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily10Y1 <- df[df$Date == "2007-02-19"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily11Y1 <- df[df$Date == "2007-02-26" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily12Y1 <- df[df$Date == "2007-03-05" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily13Y1 <- df[df$Date == "2007-03-12" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily14Y1 <- df[df$Date == "2007-03-19" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily15Y1 <- df[df$Date == "2007-03-26" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily16Y1 <- df[df$Date == "2007-04-02" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily17Y1 <- df[df$Date == "2007-04-09"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily18Y1 <- df[df$Date == "2007-04-16"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily19Y1 <- df[df$Date == "2007-04-23"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily20Y1 <- df[df$Date == "2007-04-30"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily21Y1 <- df[df$Date == "2007-05-07"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily22Y1 <- df[df$Date == "2007-05-14" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily23Y1 <- df[df$Date == "2007-05-21"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily24Y1 <- df[df$Date == "2007-05-29"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily25Y1 <- df[df$Date == "2007-06-04"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily26Y1 <- df[df$Date == "2007-06-11" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily27Y1 <- df[df$Date == "2007-06-18" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily28Y1 <- df[df$Date == "2007-06-25" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily29Y1 <- df[df$Date == "2007-07-02" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily30Y1 <- df[df$Date == "2007-07-09" & hour(df$Time) > 5 & hour(df$Time) < 9,]


dfDaily31Y1 <- df[df$Date == "2007-07-16" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily32Y1 <- df[df$Date == "2007-07-23"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily33Y1 <- df[df$Date == "2007-07-30"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily34Y1 <- df[df$Date == "2007-08-06"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily35Y1 <- df[df$Date == "2007-08-13"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily36Y1 <- df[df$Date == "2007-08-20"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily37Y1 <- df[df$Date == "2007-08-27" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily38Y1 <- df[df$Date == "2007-09-03"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily39Y1 <- df[df$Date == "2007-09-10"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily40Y1 <- df[df$Date == "2007-09-17"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily41Y1 <- df[df$Date == "2007-09-24" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily42Y1 <- df[df$Date == "2007-10-01" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily43Y1 <- df[df$Date == "2007-10-08" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily44Y1 <- df[df$Date == "2007-10-15" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily45Y1 <- df[df$Date == "2007-10-22" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily46Y1 <- df[df$Date == "2007-10-29" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily47Y1 <- df[df$Date == "2007-11-05" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily48Y1 <- df[df$Date == "2007-11-12" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily49Y1 <- df[df$Date == "2007-11-19" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily50Y1 <- df[df$Date == "2007-11-26" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily51Y1 <- df[df$Date == "2007-12-03" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily52Y1 <- df[df$Date == "2007-12-10" & hour(df$Time) > 5 & hour(df$Time) < 9,]





########### END OF YEAR 1


dfAll <- bind_rows(dfDaily1Y1, dfDaily2Y1)
dfAll <- bind_rows(dfAll, dfDaily3Y1)
dfAll <- bind_rows(dfAll, dfDaily4Y1)
dfAll <- bind_rows(dfAll, dfDaily5Y1)

dfAll <- bind_rows(dfAll, dfDaily6Y1)
dfAll <- bind_rows(dfAll, dfDaily7Y1)
dfAll <- bind_rows(dfAll, dfDaily8Y1)
dfAll <- bind_rows(dfAll, dfDaily9Y1)
dfAll <- bind_rows(dfAll, dfDaily10Y1)

dfAll <- bind_rows(dfAll, dfDaily11Y1)
dfAll <- bind_rows(dfAll, dfDaily12Y1)
dfAll <- bind_rows(dfAll, dfDaily13Y1)
dfAll <- bind_rows(dfAll, dfDaily14Y1)
dfAll <- bind_rows(dfAll, dfDaily15Y1)

dfAll <- bind_rows(dfAll, dfDaily16Y1)
dfAll <- bind_rows(dfAll, dfDaily17Y1)
dfAll <- bind_rows(dfAll, dfDaily18Y1)
dfAll <- bind_rows(dfAll, dfDaily19Y1)
dfAll <- bind_rows(dfAll, dfDaily20Y1)

dfAll <- bind_rows(dfAll, dfDaily21Y1)
dfAll <- bind_rows(dfAll, dfDaily22Y1)
dfAll <- bind_rows(dfAll, dfDaily23Y1)
dfAll <- bind_rows(dfAll, dfDaily24Y1)
dfAll <- bind_rows(dfAll, dfDaily25Y1)

dfAll <- bind_rows(dfAll, dfDaily26Y1)
dfAll <- bind_rows(dfAll, dfDaily27Y1)
dfAll <- bind_rows(dfAll, dfDaily28Y1)
dfAll <- bind_rows(dfAll, dfDaily29Y1)
dfAll <- bind_rows(dfAll, dfDaily30Y1)

dfAll <- bind_rows(dfAll, dfDaily31Y1)
dfAll <- bind_rows(dfAll, dfDaily32Y1)
dfAll <- bind_rows(dfAll, dfDaily33Y1)
dfAll <- bind_rows(dfAll, dfDaily34Y1)
dfAll <- bind_rows(dfAll, dfDaily35Y1)

dfAll <- bind_rows(dfAll, dfDaily36Y1)
dfAll <- bind_rows(dfAll, dfDaily37Y1)
dfAll <- bind_rows(dfAll, dfDaily38Y1)
dfAll <- bind_rows(dfAll, dfDaily39Y1)
dfAll <- bind_rows(dfAll, dfDaily40Y1)

dfAll <- bind_rows(dfAll, dfDaily41Y1)
dfAll <- bind_rows(dfAll, dfDaily42Y1)
dfAll <- bind_rows(dfAll, dfDaily43Y1)
dfAll <- bind_rows(dfAll, dfDaily44Y1)
dfAll <- bind_rows(dfAll, dfDaily45Y1)

dfAll <- bind_rows(dfAll, dfDaily46Y1)
dfAll <- bind_rows(dfAll, dfDaily47Y1)
dfAll <- bind_rows(dfAll, dfDaily48Y1)
dfAll <- bind_rows(dfAll, dfDaily49Y1)
dfAll <- bind_rows(dfAll, dfDaily50Y1)

dfAll <- bind_rows(dfAll, dfDaily51Y1)
dfAll <- bind_rows(dfAll, dfDaily52Y1)






  #LOADING N TIMES
ntim <- read.csv("ntimes.csv" , header = TRUE, sep = ",")
ntim <- as.numeric(ntim$ï..NTIMES)



#mod1 <- depmix(Global_active_power~1, data = dfDaily1, nstates = 20, ntimes= c(1800))

for(i in 4:14)
{
  print(`i`)
  mod<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfAll, nstates = i,family=list(gaussian(),multinomial("identity")), ntimes = ntim)
  fm <- fit(mod)
  print(fm)
  
}


mod<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfAll, nstates = 9,family=list(gaussian(),multinomial("identity")), ntimes = ntim)
fm <- fit(mod)
print(fm)

fm

for(i in 4:24)
{
  print(`i`)
  mod<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfAll, nstates = `i`,family=list(gaussian(),multinomial("identity")), ntimes = ntim)
  fm <- fit(mod)
  print(fm)
}




######
#HMM DETECTION


dfAnomaly <- read.table("DataWithAnomalies1.txt" , header = TRUE, sep = ",")
dfAnomaly$Global_intensity <- na.kalman(dfAnomaly$Global_intensity)

dfAnomaly$Date<- as.Date(dmy(dfAnomaly$Date))
dfAnomaly$Time <- dfAnomaly$Time <- as_datetime(dfAnomaly$Date + hms(dfAnomaly$Time))


####### THIS FUNCTION CUTS THE DATES OF THE DATA. #############

weeks_dfAnomaly = dfAnomaly %>% 
  mutate(weekAnomaly = cut.Date(Date, breaks = "1 week", labels = FALSE))

list_of_dfs2 <- split(weeks_dfAnomaly, weeks_dfAnomaly$week)
list2env2(list_of_dfs2, envir=.GlobalEnv)

################################################################

mod<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfAll, nstates = 11,family=list(gaussian(),multinomial("identity")), ntimes = ntim)
set.seed(2)
fm <- fit(mod)
fb <- forwardbackward(mod)
fb$logLike
print(fm)



plot(list_of_dfs2$`14`$Global_intensity, type='l')

view(list_of_dfs2$`14`)

head(dfAnomaly)
dfDailyAnomaly <- dfAnomaly[dfAnomaly$Date == "2010-03-01" & dfAnomaly$Date <= "2010-03-05"  & hour(dfAnomaly$Time) > 5 & hour(dfAnomaly$Time) < 9,]

modAnomaly<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfDailyAnomaly, nstates = 11,family=list(gaussian(),multinomial("identity")), ntimes = c(900))
fmAnomaly <- fit(modAnomaly)


summary(fmAnomaly)
summary(mod)

smamod <- zoo(list_of_dfs$`14`$Global_intensity)
smaanomaly <- zoo(list_of_dfs2$`14`$Global_intensity)
plot(list_of_dfs2$`14`$Global_intensity, type = 'l')

sma1 <- rollmean(smaanomaly,20)
sma2 <- rollmean(smamod,20)

autoplot(sma1)
autoplot(sma2)
