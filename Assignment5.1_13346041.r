d <- read_csv("ExamDataNarrow.csv")

MarksBand<- 0

for( i in 1:nrow(d)) {
  
  if (d[i,"Grade"]>=70){
    MarksBand[i]<-"H1"
  } else if (d[i,"Grade"]>=60 & d[i,"Grade"]<70){
    MarksBand[i]<-"H2.1"
  } else if (d[i,"Grade"]>=50 & d[i,"Grade"]<60){
    MarksBand[i] <- "H2.2"
  } else if (d[i,"Grade"]>=40 & d[i,"Grade"]<50){
    MarksBand[i] <- "Pass"
  } else if  (d[i,"Grade"]>=35 & d[i,"Grade"]<40){
    MarksBand[i] <- "Comp"
  } else
    MarksBand[i] <- "Fail"
  }

d <- mutate( d, MarksBand )

d<-mutate(d,
       MarksBand = case_when(
         d$Grade >= 70 ~ "H1",
         d$Grade >= 60 & d$Grade < 70 ~ "H2.1",
         d$Grade >= 50 & d$Grade < 60 ~ "H2.2",
         d$Grade >= 40 & d$Grade < 50 ~ "Pass",
         d$Grade >= 35 & d$Grade < 40 ~ "Comp",
         d$Grade < 35 ~ "Fail" 
         )
      )



ggplot(d)+
  geom_point(mapping = aes(x=StudentID, y=Grade, colour=MarksBand))+
  facet_wrap(~Subject)


d %>% group_by(StudentID) %>% summarise(
  
  Average = mean(Grade),
  Range = max(Grade) - min(Grade),
  NumH1.1 = sum(MarksBand=="H1"),
  NumH2.1 = sum(MarksBand=="H2.1"),
  NumH2.2 = sum(MarksBand=="H2.2"),
  NumPass = sum(MarksBand=="Pass"),
  NumComp = sum(MarksBand=="Comp"),
  NumFail = sum(MarksBand=="Fail")
  
) %>% 
  
  arrange(desc(Average))
  
  
d %>% group_by(Subject) %>% summarise(
  
  Average = mean(Grade),
  Range = max(Grade) - min(Grade),
  NumH1.1 = sum(MarksBand=="H1"),
  NumH2.1 = sum(MarksBand=="H2.1"),
  NumH2.2 = sum(MarksBand=="H2.2"),
  NumPass = sum(MarksBand=="Pass"),
  NumComp = sum(MarksBand=="Comp"),
  NumFail = sum(MarksBand=="Fail")
  
) %>% 
  
  arrange(desc(Average))

