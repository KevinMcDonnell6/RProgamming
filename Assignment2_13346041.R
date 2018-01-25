lm(data=faithful)

f<- function(x){
  if(!is.numeric(x)){
    stop("Error, type should be numeric")}
  
  else if(sum(is.na(x))>0){
    stop("Error, cannot have NAs!")}
  
  else
    list(Data=x,Min=min(x),Max=max(x),Mean=mean(x))
}
