#load appropriate packages
library(tidyr)
library(lubridate)
library(dplyr)
library(readr)

#Import file "process_mining" as a tibble
pm <- read_csv("process_mining.csv")

#create new tibble where allthe columns are seperate ds(data seperate)
ds <- separate(data=pm,col= "CaseID;EventID;TimeStamp;Activity;Resource;Costs",
            into = c("CaseID","EventID","TimeStamp","Activity",
                          "Resource","Costs"), sep = ";")

#replace the original "TimeStamp column with one where the class is "dttm"
ds[,"TimeStamp"] <- dmy_hm(ds$TimeStamp)

#create object TimeDif
#Although I was advised that I did not need to do this R always asks me to
TimeDif<-0

#create loop that computes the time stamp difference between an activity and its predecessor
#if the activity is "register request" assign value NA as this does not have a duration
for( i in 1:length(pull(ds[,"Activity"]))){
  TimeDif[i] <- ifelse(ds[i,"Activity"]== "register request", 
                       NA, difftime(pull(ds[i,"TimeStamp"]),pull(ds[i-1,"TimeStamp"]),
                                    units=c("days")))
}

#add new column time difference to ds
ds <- mutate(ds,TimeDif)

#create new tibble where ds is grouped by activity not caseID
byAct <- group_by(ds,Activity)

#feed this new tibble into summarise function to get desired attibutes
#then arrange by descending average duration
act <- summarise(byAct,AverageDuration = mean(TimeDif,na.rm = T),
                 "SD" = sd(TimeDif, na.rm=T), MaxTime= max(TimeDif),
                 MinTime= min(TimeDif)) %>% arrange(desc(AverageDuration))

#finally remove register request
act <- act[complete.cases(act),]
