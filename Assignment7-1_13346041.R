library(tidyr)
library(lubridate)
library(dplyr)
library(readr)
pm <- read_csv("process_mining.csv")

ds <- separate(data=pm,col= "CaseID;EventID;TimeStamp;Activity;Resource;Costs",
            into = c("CaseID","EventID","TimeStamp","Activity",
                          "Resource","Costs"), sep = ";")

ds <- separate(ds,"TimeStamp", into=c("Date","Time"), sep=":", convert = T)

ds[,"Date"] <-as_tibble(dmy(pull(ds,"Date")))
ds[,"Time"] <-as_tibble(hm(pull(ds,"Time")))

ds <- unite(ds,col= "time",Date,Time,sep=" ",remove=T)
ds[,"time"] <-as_tibble(ymd_hms(pull(ds,"time")))

spread(data = ds, key = CaseID, value= Activity)

difftime(pull(ds[1,"time"]),pull(ds[2,"time"]),tz="UTC",units =c("days"))

ds1 <- filter(ds,CaseID==1)
ds2 <- filter(ds,CaseID==2)
ds3 <- filter(ds,CaseID==3)
ds4 <- filter(ds,CaseID==4)
ds5 <- filter(ds,CaseID==5)
ds6 <- filter(ds,CaseID==6)

td1 <- 0
td2 <- 0
td3 <- 0
td4 <- 0
td5 <- 0
td6 <- 0

for (i in 2:length(pull(ds1[,"CaseID"]))){
  td1[i] <- difftime(pull(ds1[i,"time"]),pull(ds1[i-1,"time"]),units =c("days"))
}
for (i in 2:length(pull(ds2[,"CaseID"]))){
  td2[i] <- difftime(pull(ds2[i,"time"]),pull(ds2[i-1,"time"]),units =c("days"))
}
for (i in 2:length(pull(ds3[,"CaseID"]))){
  td3[i] <- difftime(pull(ds3[i,"time"]),pull(ds3[i-1,"time"]),units =c("days"))
}
for (i in 2:length(pull(ds4[,"CaseID"]))){
  td4[i] <- difftime(pull(ds4[i,"time"]),pull(ds4[i-1,"time"]),units =c("days"))
}
for (i in 2:length(pull(ds5[,"CaseID"]))){
  td5[i] <- difftime(pull(ds5[i,"time"]),pull(ds5[i-1,"time"]),units =c("days"))
}
for (i in 2:length(pull(ds6[,"CaseID"]))){
  td6[i] <- difftime(pull(ds6[i,"time"]),pull(ds6[i-1,"time"]),units =c("days"))
}

activ <- tibble(Activity=c("pay compensation","decide","examine casually",
                     "check ticket","register request","examine thoroughly",
                     "reinitiate request", "reject request"))


tib1 <- bind_cols(ds1[,"Activity"],"td1"=tibble(v1=td1))
tib2 <- bind_cols(ds2[,"Activity"],"td2"=tibble(v2=td2))
tib3 <- bind_cols(ds3[,"Activity"],"td3"=tibble(v3=td3))
tib4 <- bind_cols(ds4[,"Activity"],"td4"=tibble(v4=td4))
tib5 <- bind_cols(ds5[,"Activity"],"td5"=tibble(v5=td5))
tib6 <- bind_cols(ds6[,"Activity"],"td6"=tibble(v6=td6))
                 
tib <- left_join(activ,tib1)
tib <- left_join(tib,tib2,key="Activity")
tib <- left_join(tib,tib3,key="Activity")
tib <- left_join(tib,tib4,key="Activity")
tib <- left_join(tib,tib5,key="Activity")
tib <- left_join(tib,tib6,key="Activity")             

tib1 <- bind_cols(ds1[,"Activity"],as_tibble(td1))
tib2 <- bind_cols(ds2[,"Activity"],as_tibble(td2))
tib3 <- bind_cols(ds3[,"Activity"],as_tibble(td3))
tib4 <- bind_cols(ds4[,"Activity"],as_tibble(td4))
tib5 <- bind_cols(ds5[,"Activity"],as_tibble(td5))
tib6 <- bind_cols(ds6[,"Activity"],as_tibble(td6))

t <- as.data.frame(tib1)
t <- rbind.data.frame(t,as.data.frame(tib2),
                      as.data.frame(tib3),as.data.frame(tib4),
                      as.data.frame(tib5),as.data.frame(tib6))

tt<- as_tibble(t)

(filter(tt,Activity=="decide"))

byAct <- group_by(tt,Activity)
act <- summarise(byAct,AverageDuration = mean(value,na.rm = T),
                 "SD" = sd(value, na.rm=T), MaxTime= max(value),
                 MinTime= min(value))


tib <- tibble(ds$CaseID, dmy_hm(ds$TimeStamp), ds$Activity)

tib %>% group_by(CaseID)

ds[,"TimeStamp"] <- dmy_hm(ds$TimeStamp)

for( i in 1:length(pull(ds[,"Activity"]))){
  TimeDif[i] <- ifelse(ds[i,"Activity"]== "register request", 
                       NA, difftime(pull(ds[i,"TimeStamp"]),pull(ds[i-1,"TimeStamp"]),
                                    units=c("days")))

}

ds <- mutate(ds,TimeDif)

byAct <- group_by(ds,Activity)
act <- summarise(byAct,AverageDuration = mean(TimeDif,na.rm = T),
                 "SD" = sd(TimeDif, na.rm=T), MaxTime= max(TimeDif),
                 MinTime= min(TimeDif)) %>% arrange(desc(AverageDuration))
act <- act[complete.cases(act),]
