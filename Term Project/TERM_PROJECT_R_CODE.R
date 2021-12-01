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

#checking if data is good.
summary(df)

#fixes NA values using kalman approximations.
df[,3:9] <- na_kalman(df[,3:9])


#scaling important variables
df$Global_active_power <- (df$Global_active_power, center= TRUE)
df$Global_intensity <- scale(df$Global_intensity, center= TRUE)


#this is the PCA analysis part
#saves data for principal analysis chart.

#ANALYSIS OF ALL DATA FROM 4 YEARS
#myPrALL <- prcomp(na.omit(df[,3:9]), scale = TRUE)
#summary(myPrALL)
#myPrALL
#head(myPrALL)

#ANALYSIS OF DATA FROM 2006 TO 2008 (3 YEARS)
df1 <- df[df$Date >= "2006-01-01" & df$Date <= "2008-12-31",]
myPr1 <- prcomp(na.omit(df1[,3:9]), scale = TRUE)
summary(myPr1)
myPr1
head(myPr1)



#converting my data to CSV for future reference and all that good good.
dfRotation1 <- data.frame(myPr1$rotation)
view(dfRotation1)
write.csv(dfRotation1,"rotation1.csv", row.names = TRUE)


pca.var.ALL <- myPrALL$sdev^2
pca.var.per.ALL <- round(pca.var.ALL/sum(pca.var.ALL)*100, 1)


barplot(pca.var.per.ALL ,names.arg=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7"))
autoplot(myPr1)

##########################################################
#              HMM Modeling and TESTING                  #
#                     PART  2                            #
##########################################################

####### THIS FUNCTION CUTS THE DATES OF THE DATA. #############

weeks_df = df %>% 
  mutate(week = cut.Date(Date, breaks = "1 week", labels = FALSE))

list_of_dfs <- split(weeks_df, weeks_df$week)
list2env(list_of_dfs, envir=.GlobalEnv)

################################################################


#########################################################
##                START OF TRAIN DATA            ########
#########################################################

##### TRAIN DATA ########

##### YEAR 1 ########


dfDaily1Y1 <- df[df$Date == "2006-12-18" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily2Y1 <- df[df$Date == "2006-12-25"   & hour(df$Time) > 5 & hour(df$Time) < 9,]


##### YEAR 2 ########

dfDaily1Y2 <- df[df$Date == "2007-01-01"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily2Y2 <- df[df$Date == "2007-01-08"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily3Y2 <- df[df$Date == "2007-01-15"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily4Y2 <- df[df$Date == "2007-01-22"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily5Y2 <- df[df$Date == "2007-01-29" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily6Y2 <- df[df$Date == "2007-02-05"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily7Y2 <- df[df$Date == "2007-02-12"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily8Y2 <- df[df$Date == "2007-02-19"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily9Y2 <- df[df$Date == "2007-02-26" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily10Y2 <- df[df$Date == "2007-03-05" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily11Y2 <- df[df$Date == "2007-03-12" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily12Y2 <- df[df$Date == "2007-03-19" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily13Y2 <- df[df$Date == "2007-03-26" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily14Y2 <- df[df$Date == "2007-04-02" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily15Y2 <- df[df$Date == "2007-04-09"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily16Y2 <- df[df$Date == "2007-04-16"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily17Y2 <- df[df$Date == "2007-04-23"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily18Y2 <- df[df$Date == "2007-04-30"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily19Y2 <- df[df$Date == "2007-05-07"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily20Y2 <- df[df$Date == "2007-05-14" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily21Y2 <- df[df$Date == "2007-05-21"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily22Y2 <- df[df$Date == "2007-05-29"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily23Y2 <- df[df$Date == "2007-06-04"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily24Y2 <- df[df$Date == "2007-06-11" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily25Y2 <- df[df$Date == "2007-06-18" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily26Y2 <- df[df$Date == "2007-06-25" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily27Y2 <- df[df$Date == "2007-07-02" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily28Y2 <- df[df$Date == "2007-07-09" & hour(df$Time) > 5 & hour(df$Time) < 9,]


dfDaily29Y2 <- df[df$Date == "2007-07-16" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily30Y2 <- df[df$Date == "2007-07-23"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily31Y2 <- df[df$Date == "2007-07-30"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily32Y2 <- df[df$Date == "2007-08-06"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily33Y2 <- df[df$Date == "2007-08-13"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily34Y2 <- df[df$Date == "2007-08-20"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily35Y2 <- df[df$Date == "2007-08-27" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily36Y2 <- df[df$Date == "2007-09-03"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily37Y2 <- df[df$Date == "2007-09-10"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily38Y2 <- df[df$Date == "2007-09-17"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily39Y2 <- df[df$Date == "2007-09-24" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily40Y2 <- df[df$Date == "2007-10-01" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily41Y2 <- df[df$Date == "2007-10-08" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily42Y2 <- df[df$Date == "2007-10-15" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily43Y2 <- df[df$Date == "2007-10-22" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily44Y2 <- df[df$Date == "2007-10-29" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily45Y2 <- df[df$Date == "2007-11-05" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily46Y2 <- df[df$Date == "2007-11-12" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily47Y2 <- df[df$Date == "2007-11-19" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily48Y2 <- df[df$Date == "2007-11-26" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily49Y2 <- df[df$Date == "2007-12-03" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily50Y2 <- df[df$Date == "2007-12-10" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily51Y2 <- df[df$Date == "2007-12-17" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily52Y2 <- df[df$Date == "2007-12-24" & hour(df$Time) > 5 & hour(df$Time) < 9,]


########### END OF YEAR 2

##### YEAR 2 ########

dfDaily1Y3 <- df[df$Date == "2007-12-31" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily2Y3 <- df[df$Date == "2008-01-07"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily3Y3 <- df[df$Date == "2008-01-14"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily4Y3 <- df[df$Date == "2008-01-21"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily5Y3 <- df[df$Date == "2008-01-28" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily6Y3 <- df[df$Date == "2008-02-04"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily7Y3 <- df[df$Date == "2008-02-11"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily8Y3 <- df[df$Date == "2008-02-18"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily9Y3 <- df[df$Date == "2008-02-25" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily10Y3 <- df[df$Date == "2008-03-03" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily11Y3 <- df[df$Date == "2008-03-10" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily12Y3 <- df[df$Date == "2008-03-17" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily13Y3 <- df[df$Date == "2008-03-24" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily14Y3 <- df[df$Date == "2008-03-31" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily15Y3 <- df[df$Date == "2008-04-07"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily16Y3 <- df[df$Date == "2008-04-14"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily17Y3 <- df[df$Date == "2008-04-21"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily18Y3 <- df[df$Date == "2008-04-28"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily19Y3 <- df[df$Date == "2008-05-05"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily20Y3 <- df[df$Date == "2008-05-12" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily21Y3 <- df[df$Date == "2008-05-19"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily22Y3 <- df[df$Date == "2008-05-26"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily23Y3 <- df[df$Date == "2008-06-02"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily24Y3 <- df[df$Date == "2008-06-09" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily25Y3 <- df[df$Date == "2008-06-16" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily26Y3 <- df[df$Date == "2008-06-23" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily27Y3 <- df[df$Date == "2008-06-30" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily28Y3 <- df[df$Date == "2008-07-07" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily29Y3 <- df[df$Date == "2008-07-14" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily30Y3 <- df[df$Date == "2008-07-21"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily31Y3 <- df[df$Date == "2008-07-28"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily32Y3 <- df[df$Date == "2008-08-04"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily33Y3 <- df[df$Date == "2008-08-11"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily34Y3 <- df[df$Date == "2008-08-18"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily35Y3 <- df[df$Date == "2008-08-25" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily36Y3 <- df[df$Date == "2008-09-01"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily37Y3 <- df[df$Date == "2008-09-08"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily38Y3 <- df[df$Date == "2008-09-15"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily39Y3 <- df[df$Date == "2008-09-22" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily40Y3 <- df[df$Date == "2008-09-29" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily41Y3 <- df[df$Date == "2008-10-06" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily42Y3 <- df[df$Date == "2008-10-13" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily43Y3 <- df[df$Date == "2008-10-20" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily44Y3 <- df[df$Date == "2008-10-27" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily45Y3 <- df[df$Date == "2008-11-03" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily46Y3 <- df[df$Date == "2008-11-10" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily47Y3 <- df[df$Date == "2008-11-17" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily48Y3 <- df[df$Date == "2008-11-24" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily49Y3 <- df[df$Date == "2008-12-01" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily50Y3 <- df[df$Date == "2008-12-08" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily51Y3 <- df[df$Date == "2008-12-15" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily52Y3 <- df[df$Date == "2008-12-22" & hour(df$Time) > 5 & hour(df$Time) < 9,]


########### END OF YEAR 3


#########################################################
##               END OF TRAIN DATA               ########
#########################################################



#########################################################
##               START OF TEST DATA              ########
#########################################################




dfDaily1Y4 <- df[df$Date == "2008-12-29" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily2Y4 <- df[df$Date == "2009-01-05"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily3Y4 <- df[df$Date == "2009-01-12"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily4Y4 <- df[df$Date == "2009-01-19"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily5Y4 <- df[df$Date == "2009-01-26" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily6Y4 <- df[df$Date == "2009-02-02"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily7Y4 <- df[df$Date == "2009-02-09"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily8Y4 <- df[df$Date == "2009-02-16"  & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily9Y4 <- df[df$Date == "2009-02-23" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily10Y4 <- df[df$Date == "2009-03-02" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily11Y4 <- df[df$Date == "2009-03-09" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily12Y4 <- df[df$Date == "2009-03-16" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily13Y4 <- df[df$Date == "2009-03-23" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily14Y4 <- df[df$Date == "2009-03-30" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily15Y4 <- df[df$Date == "2009-04-06"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily16Y4 <- df[df$Date == "2009-04-13"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily17Y4 <- df[df$Date == "2009-04-20"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily18Y4 <- df[df$Date == "2009-04-27"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily19Y4 <- df[df$Date == "2009-05-04"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily20Y4 <- df[df$Date == "2009-05-11" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily21Y4 <- df[df$Date == "2009-05-18"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily22Y4 <- df[df$Date == "2009-05-25"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily23Y4 <- df[df$Date == "2009-06-01"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily24Y4 <- df[df$Date == "2009-06-08" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily25Y4 <- df[df$Date == "2009-06-15" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily26Y4 <- df[df$Date == "2009-06-22" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily27Y4 <- df[df$Date == "2009-06-29" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily28Y4 <- df[df$Date == "2009-07-06" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily29Y4 <- df[df$Date == "2009-07-13" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily30Y4 <- df[df$Date == "2009-07-20"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily31Y4 <- df[df$Date == "2009-07-27"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily32Y4 <- df[df$Date == "2009-08-03"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily33Y4 <- df[df$Date == "2009-08-10"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily34Y4 <- df[df$Date == "2009-08-17"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily35Y4 <- df[df$Date == "2009-08-24" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily36Y4 <- df[df$Date == "2009-08-31"   & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily37Y4 <- df[df$Date == "2009-09-07"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily38Y4 <- df[df$Date == "2009-09-14"  & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily39Y4 <- df[df$Date == "2009-09-21" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily40Y4 <- df[df$Date == "2009-09-28" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily41Y4 <- df[df$Date == "2009-10-05" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily42Y4 <- df[df$Date == "2009-10-12" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily43Y4 <- df[df$Date == "2009-10-19" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily44Y4 <- df[df$Date == "2009-10-26" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily45Y4 <- df[df$Date == "2009-11-02" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily46Y4 <- df[df$Date == "2009-11-09" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily47Y4 <- df[df$Date == "2009-11-16" & hour(df$Time) > 5 & hour(df$Time) < 9,]
dfDaily48Y4 <- df[df$Date == "2009-11-23" & hour(df$Time) > 5 & hour(df$Time) < 9,]

dfDaily49Y4 <- df[df$Date == "2009-11-30" & hour(df$Time) > 5 & hour(df$Time) < 9,]


########### END OF YEAR 4


#########################################################
##                START  OF TRAIN DATA           ########
#########################################################

dfTest <- bind_rows(dfDaily1Y1, dfDaily2Y1)

dfTest <- bind_rows(dfTest, dfDaily1Y2)
dfTest <- bind_rows(dfTest, dfDaily2Y2)
dfTest <- bind_rows(dfTest, dfDaily3Y2)
dfTest <- bind_rows(dfTest, dfDaily4Y2)
dfTest <- bind_rows(dfTest, dfDaily5Y2)

dfTest <- bind_rows(dfTest, dfDaily6Y2)
dfTest <- bind_rows(dfTest, dfDaily7Y2)
dfTest <- bind_rows(dfTest, dfDaily8Y2)
dfTest <- bind_rows(dfTest, dfDaily9Y2)
dfTest <- bind_rows(dfTest, dfDaily10Y2)

dfTest <- bind_rows(dfTest, dfDaily11Y2)
dfTest <- bind_rows(dfTest, dfDaily12Y2)
dfTest <- bind_rows(dfTest, dfDaily13Y2)
dfTest <- bind_rows(dfTest, dfDaily14Y2)
dfTest <- bind_rows(dfTest, dfDaily15Y2)

dfTest <- bind_rows(dfTest, dfDaily16Y2)
dfTest <- bind_rows(dfTest, dfDaily17Y2)
dfTest <- bind_rows(dfTest, dfDaily18Y2)
dfTest <- bind_rows(dfTest, dfDaily19Y2)
dfTest <- bind_rows(dfTest, dfDaily20Y2)

dfTest <- bind_rows(dfTest, dfDaily21Y2)
dfTest <- bind_rows(dfTest, dfDaily22Y2)
dfTest <- bind_rows(dfTest, dfDaily23Y2)
dfTest <- bind_rows(dfTest, dfDaily24Y2)
dfTest <- bind_rows(dfTest, dfDaily25Y2)

dfTest <- bind_rows(dfTest, dfDaily26Y2)
dfTest <- bind_rows(dfTest, dfDaily27Y2)
dfTest <- bind_rows(dfTest, dfDaily28Y2)
dfTest <- bind_rows(dfTest, dfDaily29Y2)
dfTest <- bind_rows(dfTest, dfDaily30Y2)

dfTest <- bind_rows(dfTest, dfDaily31Y2)
dfTest <- bind_rows(dfTest, dfDaily32Y2)
dfTest <- bind_rows(dfTest, dfDaily33Y2)
dfTest <- bind_rows(dfTest, dfDaily34Y2)
dfTest <- bind_rows(dfTest, dfDaily35Y2)

dfTest <- bind_rows(dfTest, dfDaily36Y2)
dfTest <- bind_rows(dfTest, dfDaily37Y2)
dfTest <- bind_rows(dfTest, dfDaily38Y2)
dfTest <- bind_rows(dfTest, dfDaily39Y2)
dfTest <- bind_rows(dfTest, dfDaily40Y2)
dfTest <- bind_rows(dfTest, dfDaily41Y2)
dfTest <- bind_rows(dfTest, dfDaily42Y2)
dfTest <- bind_rows(dfTest, dfDaily43Y2)
dfTest <- bind_rows(dfTest, dfDaily44Y2)
dfTest <- bind_rows(dfTest, dfDaily45Y2)

dfTest <- bind_rows(dfTest, dfDaily46Y2)
dfTest <- bind_rows(dfTest, dfDaily47Y2)
dfTest <- bind_rows(dfTest, dfDaily48Y2)
dfTest <- bind_rows(dfTest, dfDaily49Y2)
dfTest <- bind_rows(dfTest, dfDaily50Y2)

dfTest <- bind_rows(dfTest, dfDaily51Y2)
dfTest <- bind_rows(dfTest, dfDaily52Y2)

#### YEAR 3 binding

dfTest <- bind_rows(dfTest, dfDaily1Y3)
dfTest <- bind_rows(dfTest, dfDaily2Y3)
dfTest <- bind_rows(dfTest, dfDaily3Y3)
dfTest <- bind_rows(dfTest, dfDaily4Y3)
dfTest <- bind_rows(dfTest, dfDaily5Y3)

dfTest <- bind_rows(dfTest, dfDaily6Y3)
dfTest <- bind_rows(dfTest, dfDaily7Y3)
dfTest <- bind_rows(dfTest, dfDaily8Y3)
dfTest <- bind_rows(dfTest, dfDaily9Y3)
dfTest <- bind_rows(dfTest, dfDaily10Y3)

dfTest <- bind_rows(dfTest, dfDaily11Y3)
dfTest <- bind_rows(dfTest, dfDaily12Y3)
dfTest <- bind_rows(dfTest, dfDaily13Y3)
dfTest <- bind_rows(dfTest, dfDaily14Y3)
dfTest <- bind_rows(dfTest, dfDaily15Y3)

dfTest <- bind_rows(dfTest, dfDaily16Y3)
dfTest <- bind_rows(dfTest, dfDaily17Y3)
dfTest <- bind_rows(dfTest, dfDaily18Y3)
dfTest <- bind_rows(dfTest, dfDaily19Y3)
dfTest <- bind_rows(dfTest, dfDaily20Y3)

dfTest <- bind_rows(dfTest, dfDaily21Y3)
dfTest <- bind_rows(dfTest, dfDaily22Y3)
dfTest <- bind_rows(dfTest, dfDaily23Y3)
dfTest <- bind_rows(dfTest, dfDaily24Y3)
dfTest <- bind_rows(dfTest, dfDaily25Y3)

dfTest <- bind_rows(dfTest, dfDaily26Y3)
dfTest <- bind_rows(dfTest, dfDaily27Y3)
dfTest <- bind_rows(dfTest, dfDaily28Y3)
dfTest <- bind_rows(dfTest, dfDaily29Y3)
dfTest <- bind_rows(dfTest, dfDaily30Y3)

dfTest <- bind_rows(dfTest, dfDaily31Y3)
dfTest <- bind_rows(dfTest, dfDaily32Y3)
dfTest <- bind_rows(dfTest, dfDaily33Y3)
dfTest <- bind_rows(dfTest, dfDaily34Y3)
dfTest <- bind_rows(dfTest, dfDaily35Y3)

dfTest <- bind_rows(dfTest, dfDaily36Y3)
dfTest <- bind_rows(dfTest, dfDaily37Y3)
dfTest <- bind_rows(dfTest, dfDaily38Y3)
dfTest <- bind_rows(dfTest, dfDaily39Y3)
dfTest <- bind_rows(dfTest, dfDaily40Y3)
dfTest <- bind_rows(dfTest, dfDaily41Y3)
dfTest <- bind_rows(dfTest, dfDaily42Y3)
dfTest <- bind_rows(dfTest, dfDaily43Y3)
dfTest <- bind_rows(dfTest, dfDaily44Y3)
dfTest <- bind_rows(dfTest, dfDaily45Y3)

dfTest <- bind_rows(dfTest, dfDaily46Y3)
dfTest <- bind_rows(dfTest, dfDaily47Y3)
dfTest <- bind_rows(dfTest, dfDaily48Y3)
dfTest <- bind_rows(dfTest, dfDaily49Y3)
dfTest <- bind_rows(dfTest, dfDaily50Y3)

dfTest <- bind_rows(dfTest, dfDaily51Y3)
dfTest <- bind_rows(dfTest, dfDaily52Y3)

#Training binding
dfTrain <- data.frame()
dfTrain <- bind_rows(dfTrain, dfDaily1Y4)
dfTrain <- bind_rows(dfTrain, dfDaily2Y4)
dfTrain <- bind_rows(dfTrain, dfDaily3Y4)
dfTrain <- bind_rows(dfTrain, dfDaily4Y4)
dfTrain <- bind_rows(dfTrain, dfDaily5Y4)

dfTrain <- bind_rows(dfTrain, dfDaily6Y4)
dfTrain <- bind_rows(dfTrain, dfDaily7Y4)
dfTrain <- bind_rows(dfTrain, dfDaily8Y4)
dfTrain <- bind_rows(dfTrain, dfDaily9Y4)
dfTrain <- bind_rows(dfTrain, dfDaily10Y4)

dfTrain <- bind_rows(dfTrain, dfDaily11Y4)
dfTrain <- bind_rows(dfTrain, dfDaily12Y4)
dfTrain <- bind_rows(dfTrain, dfDaily13Y4)
dfTrain <- bind_rows(dfTrain, dfDaily14Y4)
dfTrain <- bind_rows(dfTrain, dfDaily15Y4)

dfTrain <- bind_rows(dfTrain, dfDaily16Y4)
dfTrain <- bind_rows(dfTrain, dfDaily17Y4)
dfTrain <- bind_rows(dfTrain, dfDaily18Y4)
dfTrain <- bind_rows(dfTrain, dfDaily19Y4)
dfTrain <- bind_rows(dfTrain, dfDaily20Y4)

dfTrain <- bind_rows(dfTrain, dfDaily21Y4)
dfTrain <- bind_rows(dfTrain, dfDaily22Y4)
dfTrain <- bind_rows(dfTrain, dfDaily23Y4)
dfTrain <- bind_rows(dfTrain, dfDaily24Y4)
dfTrain <- bind_rows(dfTrain, dfDaily25Y4)

dfTrain <- bind_rows(dfTrain, dfDaily26Y4)
dfTrain <- bind_rows(dfTrain, dfDaily27Y4)
dfTrain <- bind_rows(dfTrain, dfDaily28Y4)
dfTrain <- bind_rows(dfTrain, dfDaily29Y4)
dfTrain <- bind_rows(dfTrain, dfDaily30Y4)

dfTrain <- bind_rows(dfTrain, dfDaily31Y4)
dfTrain <- bind_rows(dfTrain, dfDaily32Y4)
dfTrain <- bind_rows(dfTrain, dfDaily33Y4)
dfTrain <- bind_rows(dfTrain, dfDaily34Y4)
dfTrain <- bind_rows(dfTrain, dfDaily35Y4)

dfTrain <- bind_rows(dfTrain, dfDaily36Y4)
dfTrain <- bind_rows(dfTrain, dfDaily37Y4)
dfTrain <- bind_rows(dfTrain, dfDaily38Y4)
dfTrain <- bind_rows(dfTrain, dfDaily39Y4)
dfTrain <- bind_rows(dfTrain, dfDaily40Y4)

dfTrain <- bind_rows(dfTrain, dfDaily41Y4)
dfTrain <- bind_rows(dfTrain, dfDaily42Y4)
dfTrain <- bind_rows(dfTrain, dfDaily43Y4)
dfTrain <- bind_rows(dfTrain, dfDaily44Y4)
dfTrain <- bind_rows(dfTrain, dfDaily45Y4)

dfTrain <- bind_rows(dfTrain, dfDaily46Y4)
dfTrain <- bind_rows(dfTrain, dfDaily47Y4)
dfTrain <- bind_rows(dfTrain, dfDaily48Y4)
dfTrain <- bind_rows(dfTrain, dfDaily49Y4)


#LOADING N TIMES
ntim <- read.csv("ntimes.csv" , header = TRUE, sep = ",")
ntim <- as.numeric(ntim$ï..NTIMES)

ntim2 <- read.csv("ntimes2.csv" , header = TRUE, sep = ",")
ntim2 <- as.numeric(ntim2$ï..NTIMES)


ntimAnomaly <- read.csv("ntimesANOMALY.csv" , header = TRUE, sep = ",")
ntimAnomaly <- as.numeric(ntimAnomaly$ï..NTIMES)




#mod1 <- depmix(Global_active_power~1, data = dfDaily1, nstates = 20, ntimes= c(1800))

for(i in 4:24)
{
  print("TRAIN")
  print(`i`)
  mod<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfTest, nstates = i,family=list(gaussian(),gaussian()), ntimes = ntim)
  fm <- fit(mod)
  fb <- forwardbackward(mod)
  print(fm)
  print(fb)
  
  print("TEST")
  print(`i`)
  mod2<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfTrain, nstates = i,family=list(gaussian(),gaussian()), ntimes = ntim2)
  fm2 <- fit(mod2)
  fb <- forwardbackward(mod2)
  print(fm2)
  print(fb)
  
}


mod<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfTest, nstates = 10,family=list(gaussian(),gaussian()), ntimes = ntim)
fm <- fit(mod)
print(fm)


mod2<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfTrain, nstates = 10,family=list(gaussian(),gaussian()), ntimes = ntim2)
fm2 <- fit(mod2)
print(fm2)




##########################################################
#              ANOMALY DETECTION                         #
#                     PART  3                            #
##########################################################

#get anomaly dataframes.
dfAnomaly1 <- read.table("DataWithAnomalies2.txt" , header = TRUE, sep = ",")
dfAnomaly2 <- read.table("DataWithAnomalies2.txt" , header = TRUE, sep = ",")
dfAnomaly3 <- read.table("DataWithAnomalies3.txt" , header = TRUE, sep = ",")


#FIXES NA VALUES, CONVERTS DATE TO OBJECTS, SCALE IMPORTANT DATA

dfAnomaly1$Date <- as.Date(dmy(dfAnomaly1$Date))
dfAnomaly1$Time <- dfAnomaly1$Time <- as_datetime(dfAnomaly1$Date + hms(dfAnomaly1$Time))
dfAnomaly2$Date <- as.Date(dmy(dfAnomaly2$Date))
dfAnomaly2$Time <- as_datetime(dfAnomaly2$Date + hms(dfAnomaly2$Time))
dfAnomaly3$Date <- as.Date(dmy(dfAnomaly3$Date))
dfAnomaly3$Time <- as_datetime(dfAnomaly3$Date + hms(dfAnomaly3$Time))


dfAnomaly1$Global_active_power <- na_kalman(dfAnomaly1$Global_active_power)
dfAnomaly1$Global_intensity <- na_kalman(dfAnomaly1$Global_intensity)

dfAnomaly2$Global_active_power <- na_kalman(dfAnomaly2$Global_active_power)
dfAnomaly2$Global_intensity <- na_kalman(dfAnomaly2$Global_intensity)

dfAnomaly3$Global_active_power <- na_kalman(dfAnomaly3$Global_active_power)
dfAnomaly3$Global_intensity <- na_kalman(dfAnomaly3$Global_intensity)




dfAnomaly1$Global_active_power <- scale(dfAnomaly1$Global_active_power)
dfAnomaly1$Global_intensity <- scale(dfAnomaly1$Global_intensity)


dfAnomaly2$Global_active_power <- scale(dfAnomaly2$Global_active_power)
dfAnomaly2$Global_intensity <- scale(dfAnomaly2$Global_intensity)

dfAnomaly3$Global_active_power <- scale(dfAnomaly3$Global_active_power)
dfAnomaly3$Global_intensity <- scale(dfAnomaly3$Global_intensity)



####### THIS FUNCTION CUTS THE DATES OF THE DATA. #############

weeks_dfAnomaly1 = dfAnomaly1 %>% 
  mutate(weekAnomaly1 = cut.Date(Date, breaks = "1 week", labels = FALSE))

list_of_dfs2 <- split(weeks_dfAnomaly1, weeks_dfAnomaly1$week)
#list2env2(list_of_dfs2, envir=.GlobalEnv)

################################################################

## DATA ANOMALY 1


dfAnomaly1w1 <- dfAnomaly1[dfAnomaly1$Date == "2009-12-07" & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w2 <- dfAnomaly1[dfAnomaly1$Date == "2009-12-14"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w3 <- dfAnomaly1[dfAnomaly1$Date == "2009-12-21"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w4 <- dfAnomaly1[dfAnomaly1$Date == "2009-12-28"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w5 <- dfAnomaly1[dfAnomaly1$Date == "2010-01-04" & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]

dfAnomaly1w6 <- dfAnomaly1[dfAnomaly1$Date == "2010-01-11"   & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w7 <- dfAnomaly1[dfAnomaly1$Date == "2010-01-18"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w8 <- dfAnomaly1[dfAnomaly1$Date == "2010-01-25"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w9 <- dfAnomaly1[dfAnomaly1$Date == "2010-02-01"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w10 <- dfAnomaly1[dfAnomaly1$Date == "2010-02-08"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]

dfAnomaly1w11 <- dfAnomaly1[dfAnomaly1$Date == "2010-02-15" & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w12 <- dfAnomaly1[dfAnomaly1$Date == "2010-02-22"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w13 <- dfAnomaly1[dfAnomaly1$Date == "2010-03-01"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w14 <- dfAnomaly1[dfAnomaly1$Date == "2010-03-08"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w15 <- dfAnomaly1[dfAnomaly1$Date == "2010-03-15" & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]

dfAnomaly1w16 <- dfAnomaly1[dfAnomaly1$Date == "2010-03-22"   & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w17 <- dfAnomaly1[dfAnomaly1$Date == "2010-03-29"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w18 <- dfAnomaly1[dfAnomaly1$Date == "2010-04-05"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w19 <- dfAnomaly1[dfAnomaly1$Date == "2010-04-12"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w20 <- dfAnomaly1[dfAnomaly1$Date == "2010-04-19"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]

dfAnomaly1w21 <- dfAnomaly1[dfAnomaly1$Date == "2010-04-26" & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w22 <- dfAnomaly1[dfAnomaly1$Date == "2010-05-03"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w23 <- dfAnomaly1[dfAnomaly1$Date == "2010-05-10"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w24 <- dfAnomaly1[dfAnomaly1$Date == "2010-05-17"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w25 <- dfAnomaly1[dfAnomaly1$Date == "2010-05-24" & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]

dfAnomaly1w26 <- dfAnomaly1[dfAnomaly1$Date == "2010-05-31"   & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w27 <- dfAnomaly1[dfAnomaly1$Date == "2010-06-07"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w28 <- dfAnomaly1[dfAnomaly1$Date == "2010-06-14"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w29 <- dfAnomaly1[dfAnomaly1$Date == "2010-06-21"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w30 <- dfAnomaly1[dfAnomaly1$Date == "2010-06-28"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]

dfAnomaly1w31 <- dfAnomaly1[dfAnomaly1$Date == "2010-07-05" & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w32 <- dfAnomaly1[dfAnomaly1$Date == "2010-07-12"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w33 <- dfAnomaly1[dfAnomaly1$Date == "2010-07-19"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w34 <- dfAnomaly1[dfAnomaly1$Date == "2010-07-28"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w35 <- dfAnomaly1[dfAnomaly1$Date == "2010-08-02" & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]

dfAnomaly1w36 <- dfAnomaly1[dfAnomaly1$Date == "2010-08-09"   & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w37 <- dfAnomaly1[dfAnomaly1$Date == "2010-08-16"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w38 <- dfAnomaly1[dfAnomaly1$Date == "2010-08-23"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w39 <- dfAnomaly1[dfAnomaly1$Date == "2010-08-30"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w40 <- dfAnomaly1[dfAnomaly1$Date == "2010-09-06"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]

dfAnomaly1w41 <- dfAnomaly1[dfAnomaly1$Date == "2010-09-13" & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w42 <- dfAnomaly1[dfAnomaly1$Date == "2010-09-20"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w43 <- dfAnomaly1[dfAnomaly1$Date == "2010-09-27"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w44 <- dfAnomaly1[dfAnomaly1$Date == "2010-10-04"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w45 <- dfAnomaly1[dfAnomaly1$Date == "2010-10-11" & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]

dfAnomaly1w46 <- dfAnomaly1[dfAnomaly1$Date == "2010-10-18"   & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w47 <- dfAnomaly1[dfAnomaly1$Date == "2010-10-25"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w48 <- dfAnomaly1[dfAnomaly1$Date == "2010-11-01"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w49 <- dfAnomaly1[dfAnomaly1$Date == "2010-11-08"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
dfAnomaly1w50 <- dfAnomaly1[dfAnomaly1$Date == "2010-11-15"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]

dfAnomaly1w51 <- dfAnomaly1[dfAnomaly1$Date == "2010-11-22"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]
#dfAnomaly1w52 <- dfAnomaly1[dfAnomaly1$Date == "2010-11-29"  & hour(dfAnomaly1$Time) > 5 & hour(dfAnomaly1$Time) < 9,]

dfAnomaly1ALL <- data.frame()
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w1)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w2)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w3)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w4)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w5)

dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w6)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w7)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w8)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w9)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w10)

dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w11)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w12)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w13)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w14)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w15)

dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w16)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w17)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w18)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w19)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w20)

dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w21)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w22)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w23)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w24)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w25)

dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w26)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w27)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w28)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w29)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w30)

dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w31)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w32)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w33)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w34)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w35)

dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w36)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w37)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w38)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w39)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w40)

dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w41)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w42)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w43)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w44)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w45)

dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w46)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w47)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w48)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w49)

dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w50)
dfAnomaly1ALL <- bind_rows(dfAnomaly1ALL, dfAnomaly1w51)

#### END OF 1/ START OF 2 ############################################################



#DATA ANOMALY 2

dfAnomaly2w1 <- dfAnomaly2[dfAnomaly2$Date == "2009-12-07" & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w2 <- dfAnomaly2[dfAnomaly2$Date == "2009-12-14"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w3 <- dfAnomaly2[dfAnomaly2$Date == "2009-12-21"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w4 <- dfAnomaly2[dfAnomaly2$Date == "2009-12-28"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w5 <- dfAnomaly2[dfAnomaly2$Date == "2010-01-04" & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]

dfAnomaly2w6 <- dfAnomaly2[dfAnomaly2$Date == "2010-01-11"   & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w7 <- dfAnomaly2[dfAnomaly2$Date == "2010-01-18"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w8 <- dfAnomaly2[dfAnomaly2$Date == "2010-01-25"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w9 <- dfAnomaly2[dfAnomaly2$Date == "2010-02-01"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w10 <- dfAnomaly2[dfAnomaly2$Date == "2010-02-08"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]

dfAnomaly2w11 <- dfAnomaly2[dfAnomaly2$Date == "2010-02-15" & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w12 <- dfAnomaly2[dfAnomaly2$Date == "2010-02-22"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w13 <- dfAnomaly2[dfAnomaly2$Date == "2010-03-01"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w14 <- dfAnomaly2[dfAnomaly2$Date == "2010-03-08"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w15 <- dfAnomaly2[dfAnomaly2$Date == "2010-03-15" & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]

dfAnomaly2w16 <- dfAnomaly2[dfAnomaly2$Date == "2010-03-22"   & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w17 <- dfAnomaly2[dfAnomaly2$Date == "2010-03-29"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w18 <- dfAnomaly2[dfAnomaly2$Date == "2010-04-05"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w19 <- dfAnomaly2[dfAnomaly2$Date == "2010-04-12"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w20 <- dfAnomaly2[dfAnomaly2$Date == "2010-04-19"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]

dfAnomaly2w21 <- dfAnomaly2[dfAnomaly2$Date == "2010-04-26" & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w22 <- dfAnomaly2[dfAnomaly2$Date == "2010-05-03"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w23 <- dfAnomaly2[dfAnomaly2$Date == "2010-05-10"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w24 <- dfAnomaly2[dfAnomaly2$Date == "2010-05-17"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w25 <- dfAnomaly2[dfAnomaly2$Date == "2010-05-24" & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]

dfAnomaly2w26 <- dfAnomaly2[dfAnomaly2$Date == "2010-05-31"   & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w27 <- dfAnomaly2[dfAnomaly2$Date == "2010-06-07"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w28 <- dfAnomaly2[dfAnomaly2$Date == "2010-06-14"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w29 <- dfAnomaly2[dfAnomaly2$Date == "2010-06-21"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w30 <- dfAnomaly2[dfAnomaly2$Date == "2010-06-28"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]

dfAnomaly2w31 <- dfAnomaly2[dfAnomaly2$Date == "2010-07-05" & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w32 <- dfAnomaly2[dfAnomaly2$Date == "2010-07-12"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w33 <- dfAnomaly2[dfAnomaly2$Date == "2010-07-19"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w34 <- dfAnomaly2[dfAnomaly2$Date == "2010-07-28"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w35 <- dfAnomaly2[dfAnomaly2$Date == "2010-08-02" & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]

dfAnomaly2w36 <- dfAnomaly2[dfAnomaly2$Date == "2010-08-09"   & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w37 <- dfAnomaly2[dfAnomaly2$Date == "2010-08-16"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w38 <- dfAnomaly2[dfAnomaly2$Date == "2010-08-23"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w39 <- dfAnomaly2[dfAnomaly2$Date == "2010-08-30"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w40 <- dfAnomaly2[dfAnomaly2$Date == "2010-09-06"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]

dfAnomaly2w41 <- dfAnomaly2[dfAnomaly2$Date == "2010-09-13" & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w42 <- dfAnomaly2[dfAnomaly2$Date == "2010-09-20"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w43 <- dfAnomaly2[dfAnomaly2$Date == "2010-09-27"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w44 <- dfAnomaly2[dfAnomaly2$Date == "2010-10-04"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w45 <- dfAnomaly2[dfAnomaly2$Date == "2010-10-11" & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]

dfAnomaly2w46 <- dfAnomaly2[dfAnomaly2$Date == "2010-10-18"   & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w47 <- dfAnomaly2[dfAnomaly2$Date == "2010-10-25"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w48 <- dfAnomaly2[dfAnomaly2$Date == "2010-11-01"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w49 <- dfAnomaly2[dfAnomaly2$Date == "2010-11-08"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
dfAnomaly2w50 <- dfAnomaly2[dfAnomaly2$Date == "2010-11-15"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]

dfAnomaly2w51 <- dfAnomaly2[dfAnomaly2$Date == "2010-11-22"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]
#dfAnomaly2w52 <- dfAnomaly2[dfAnomaly2$Date == "2010-11-29"  & hour(dfAnomaly2$Time) > 5 & hour(dfAnomaly2$Time) < 9,]

#BINDING DATA for 2nd set

dfAnomaly2ALL <- data.frame()
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w1)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w2)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w3)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w4)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w5)

dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w6)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w7)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w8)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w9)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w10)

dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w11)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w12)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w13)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w14)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w15)

dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w16)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w17)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w18)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w19)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w20)

dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w21)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w22)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w23)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w24)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w25)

dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w26)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w27)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w28)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w29)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w30)

dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w31)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w32)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w33)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w34)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w35)

dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w36)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w37)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w38)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w39)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w40)

dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w41)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w42)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w43)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w44)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w45)

dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w46)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w47)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w48)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w49)

dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w50)
dfAnomaly2ALL <- bind_rows(dfAnomaly2ALL, dfAnomaly2w51)

########## END OF DATA ANOMALY 2


#######################################################################################################################

#### END OF 2/ START OF 3 #######################

##### ANOMALY 3


dfAnomaly3w1 <- dfAnomaly3[dfAnomaly3$Date == "2009-12-07" & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w2 <- dfAnomaly3[dfAnomaly3$Date == "2009-12-14"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w3 <- dfAnomaly3[dfAnomaly3$Date == "2009-12-21"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w4 <- dfAnomaly3[dfAnomaly3$Date == "2009-12-28"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w5 <- dfAnomaly3[dfAnomaly3$Date == "2010-01-04" & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]

dfAnomaly3w6 <- dfAnomaly3[dfAnomaly3$Date == "2010-01-11"   & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w7 <- dfAnomaly3[dfAnomaly3$Date == "2010-01-18"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w8 <- dfAnomaly3[dfAnomaly3$Date == "2010-01-25"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w9 <- dfAnomaly3[dfAnomaly3$Date == "2010-02-01"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w10 <- dfAnomaly3[dfAnomaly3$Date == "2010-02-08"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]

dfAnomaly3w11 <- dfAnomaly3[dfAnomaly3$Date == "2010-02-15" & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w12 <- dfAnomaly3[dfAnomaly3$Date == "2010-02-22"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w13 <- dfAnomaly3[dfAnomaly3$Date == "2010-03-01"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w14 <- dfAnomaly3[dfAnomaly3$Date == "2010-03-08"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w15 <- dfAnomaly3[dfAnomaly3$Date == "2010-03-15" & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]

dfAnomaly3w16 <- dfAnomaly3[dfAnomaly3$Date == "2010-03-22"   & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w17 <- dfAnomaly3[dfAnomaly3$Date == "2010-03-29"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w18 <- dfAnomaly3[dfAnomaly3$Date == "2010-04-05"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w19 <- dfAnomaly3[dfAnomaly3$Date == "2010-04-12"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w20 <- dfAnomaly3[dfAnomaly3$Date == "2010-04-19"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]

dfAnomaly3w21 <- dfAnomaly3[dfAnomaly3$Date == "2010-04-26" & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w22 <- dfAnomaly3[dfAnomaly3$Date == "2010-05-03"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w23 <- dfAnomaly3[dfAnomaly3$Date == "2010-05-10"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w24 <- dfAnomaly3[dfAnomaly3$Date == "2010-05-17"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w25 <- dfAnomaly3[dfAnomaly3$Date == "2010-05-24" & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]

dfAnomaly3w26 <- dfAnomaly3[dfAnomaly3$Date == "2010-05-31"   & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w27 <- dfAnomaly3[dfAnomaly3$Date == "2010-06-07"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w28 <- dfAnomaly3[dfAnomaly3$Date == "2010-06-14"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w29 <- dfAnomaly3[dfAnomaly3$Date == "2010-06-21"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w30 <- dfAnomaly3[dfAnomaly3$Date == "2010-06-28"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]

dfAnomaly3w31 <- dfAnomaly3[dfAnomaly3$Date == "2010-07-05" & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w32 <- dfAnomaly3[dfAnomaly3$Date == "2010-07-12"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w33 <- dfAnomaly3[dfAnomaly3$Date == "2010-07-19"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w34 <- dfAnomaly3[dfAnomaly3$Date == "2010-07-28"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w35 <- dfAnomaly3[dfAnomaly3$Date == "2010-08-02" & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]

dfAnomaly3w36 <- dfAnomaly3[dfAnomaly3$Date == "2010-08-09"   & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w37 <- dfAnomaly3[dfAnomaly3$Date == "2010-08-16"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w38 <- dfAnomaly3[dfAnomaly3$Date == "2010-08-23"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w39 <- dfAnomaly3[dfAnomaly3$Date == "2010-08-30"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w40 <- dfAnomaly3[dfAnomaly3$Date == "2010-09-06"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]

dfAnomaly3w41 <- dfAnomaly3[dfAnomaly3$Date == "2010-09-13" & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w42 <- dfAnomaly3[dfAnomaly3$Date == "2010-09-20"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w43 <- dfAnomaly3[dfAnomaly3$Date == "2010-09-27"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w44 <- dfAnomaly3[dfAnomaly3$Date == "2010-10-04"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w45 <- dfAnomaly3[dfAnomaly3$Date == "2010-10-11" & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]

dfAnomaly3w46 <- dfAnomaly3[dfAnomaly3$Date == "2010-10-18"   & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w47 <- dfAnomaly3[dfAnomaly3$Date == "2010-10-25"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w48 <- dfAnomaly3[dfAnomaly3$Date == "2010-11-01"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w49 <- dfAnomaly3[dfAnomaly3$Date == "2010-11-08"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]
dfAnomaly3w50 <- dfAnomaly3[dfAnomaly3$Date == "2010-11-15"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]

dfAnomaly3w51 <- dfAnomaly3[dfAnomaly3$Date == "2010-11-22"  & hour(dfAnomaly3$Time) > 5 & hour(dfAnomaly3$Time) < 9,]


#BINDING DATA for 3rd set

dfAnomaly3ALL <- data.frame()
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w1)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w2)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w3)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w4)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w5)

dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w6)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w7)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w8)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w9)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w10)

dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w11)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w12)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w13)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w14)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w15)

dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w16)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w17)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w18)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w19)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w20)

dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w21)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w22)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w23)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w24)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w25)

dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w26)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w27)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w28)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w29)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w30)

dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w31)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w32)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w33)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w34)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w35)

dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w36)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w37)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w38)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w39)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w40)

dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w41)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w42)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w43)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w44)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w45)

dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w46)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w47)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w48)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w49)

dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w50)
dfAnomaly3ALL <- bind_rows(dfAnomaly3ALL, dfAnomaly3w51)
##### END OF DATA ANOMALY 3


#here we can compare 1 year values between anomaly and normal data
#we find that N states = 10 is the best fit but, I also like to run more numbers
#this will explain a lot more of the data as we can already see divergence of the normalized
#loglikelihood of the data. -Ali
for(i in 4:10)
{
  print("MODEL")
  print(`i`)
  mod<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfTrain, nstates = i,family=list(gaussian(),gaussian()), ntimes = ntim2)
  fm <- fit(mod)
  print(fm)
  
  print("ANOMALY DATASET 2")
  print(`i`)
  mod2<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfAnomaly2ALL, nstates = i,family=list(gaussian(),gaussian()), ntimes = ntimAnomaly)
  fm2 <- fit(mod2)
  print(fm2)

  
}

{
print("MODEL")
mod<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfTrain, nstates = 10,family=list(gaussian(),gaussian()), ntimes = ntim2)
fm <- fit(mod)
print(fm)

print("ANOMALY DATASET 2")
mod2<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfAnomaly2ALL, nstates = 10,family=list(gaussian(),gaussian()), ntimes = ntimAnomaly)
fm2 <- fit(mod2)
print(fm2)
}


modAnomaly<- depmix(list(Global_active_power~1,Global_intensity~1), data = dfDailyAnomaly, nstates = 10,family=list(gaussian(),gaussian()), ntimes = c(900))
fmAnomaly <- fit(modAnomaly)


#PLOT AREA.
#this is where I was doing my plots.


summary(fmAnomaly)
summary(mod)


smamod2 <- zoo(dfTest$Global_active_power)
smaanomaly1 <- zoo(dfAnomaly1ALL$Global_intensity)
smaanomaly2 <- zoo(dfAnomaly2ALL$Global_intensity)
smaanomaly3 <- zoo(dfAnomaly3ALL$Global_intensity)

#plot(list_of_dfs$`1`$Global_intensity, type = 'l')
smamod <- zoo(list_of_dfs$`14`$Global_intensity)
smamod <- rollmean(smamod,20)
lines(smamod,col = "red")

dfDaily1Y1 <- 

plot(list_of_dfs2$`14`$Global_intensity, type = 'l')


smaModGP <- rollmean(smamod2,30)
smaAnomaly1 <- rollmean(smaanomaly1,30)
smaAnomaly2 <- rollmean(smaanomaly2,30)
smaAnomaly3 <- rollmean(smaanomaly3,30)


autoplot(smamod)
#autoplot(smaModGP)
#autoplot(smaAnomaly2)
#autoplot(smaAnomaly3)
