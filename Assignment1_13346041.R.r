  set.seed(99)
  
  D1sum <- (sample(1:6, 1000, replace=T)) + (sample(1:6, 1000, replace=T))
  Odd1<-sum(ifelse(D1sum %% 2, 1, 0))
  Even1 <- 1000- Odd1
  ans <- c(Odd1,Even1)
  names(ans) <- c("Number Odd", "Number Even")
 
  ans1<-0
  for(i in 2:12){
    ans1[i-1]<- sum(D1sum==i)
  }
  names(ans1)<- 2:12 
 print(ans1)
 
 both<-c(letters,LETTERS,0:9)
 set.seed(99)
 ans2 <- 1:5
for(k in 1:5){
  
   ans2[k]<-paste(sample(both,10,replace=T),collapse = "")
                        }


