#Import the csv file into R as tibble
d <- read_csv("ExamDataNarrow.csv")

#Create object Marksband
MarksBand<- 0


#Add column Marksband to d
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


#Plot Grades against Student IDs spliiting up the graphs by subject
ggplot(d)+
  geom_point(mapping = aes(x=StudentID, y=Grade, colour=MarksBand))+
  facet_wrap(~Subject)

#Show a tibble that compares the grades of each student
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
  
#show a tibble that comares the grades achieved in each class  
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

