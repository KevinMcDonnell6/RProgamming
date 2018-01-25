#Create object Class
Class <- "0"

library(titanic)
#Create data frame tr
tr <- titanic_train

#loop assigns name of class to vector Class
for( i in 1:891) {
  
               if (tr[i,"Pclass"]==1){
                 Class[i]<-"First"
               } else if (tr[i,"Pclass"]==2){
                 Class[i]<-"Second"
               } else 
                    Class[i] <- "Third"
}
                  
#Create boolean representation of Survived
SurvivedFlag <- tr[,"Survived"]==1

#Add columns to tr
tr <- cbind(tr,SurvivedFlag)

tr <- cbind(tr, Class)

#Replace NA's with mean
tr[is.na(tr[,"Age"]),"Age"] <- mean(tr[,"Age"], na.rm = T)

#Create AgeCohort
AgeCohort <- 0

for( i in 1:891) {
  
  if (tr[i,"Age"]<16){
    AgeCohort[i]<-"Chidren"
  } else if (tr[i,"Age"]>=60){
    AgeCohort[i]<-"Elderly"
  } else 
    AgeCohort[i] <- "Adult"
}

#Add new column AgeCohort
tr <- cbind(tr, AgeCohort)

#Plot 1
ggplot(data=tr)+ 
geom_point(mapping = aes(x=Class, y=SurvivedFlag))+
  xlab("Travel Class") + ylab("Survived")

#Plot 2
ggplot(data=tr)+ 
  geom_point(mapping = aes(x=Class, y=SurvivedFlag), position = "jitter")+
  xlab("Travel Class") + ylab("Survived")

#Plot 3
ggplot(data=tr)+ 
  geom_point(mapping = aes(x=Class, y=SurvivedFlag, colour=Sex), position = "jitter")+
  xlab("Travel Class") + ylab("Survived")

#Plot4
ggplot(data=tr)+ 
  geom_point(mapping = aes(x=Class, y=SurvivedFlag, shape=Sex, colour=AgeCohort), position = "jitter")+
  xlab("Travel Class") + ylab("Survived")

#Plot 5
ggplot(data=tr)+ 
  geom_point(mapping = aes(x=AgeCohort, y=SurvivedFlag, colour=Class), position = "jitter")+
  xlab("Age Cohort") + ylab("Survived")

#Plot 6
ggplot(data=tr)+ 
  geom_point(mapping = aes(x=AgeCohort, y=SurvivedFlag, colour=Sex), position = "jitter")+
  xlab("Age Cohort") + ylab("Survived") +facet_grid(.~ Class )

#plot 7
ggplot(data=tr)+
  geom_bar(mapping= aes(x=SurvivedFlag, fill=Sex))+
  xlab("Survived") + ylab("Number of People")

#plot 8
ggplot(data=tr)+
  geom_bar(mapping= aes(x=SurvivedFlag, fill=Sex), position = "dodge")+
  xlab("Survived") + ylab("Number of People")

#Plot 9
ggplot(data=tr)+
  geom_bar(mapping= aes(x=AgeCohort, fill=SurvivedFlag))+
  xlab("Age Cohort") + ylab("Number of People") + facet_grid(.~Sex)

#Plot 10
ggplot(data=tr)+
  geom_bar(mapping= aes(x=SurvivedFlag, fill=AgeCohort))+
  xlab("Survived") + ylab("Number of People")+ facet_grid(.~Class)
