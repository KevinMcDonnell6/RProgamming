
#set the seed
set.seed(100)

#create matrix x
x <- matrix(sample(c(0,1),replace=T,size=100),nrow=10,ncol=10)

#set diagonal to all zeros
diag(x) <- 0

#name columns and rows
colnames(x) <- LETTERS[1:10]
rownames(x) <- LETTERS[1:10]

#function wasn't working until i created object node
node<-0

#fololows fuction
follows <- function(node){
  
  if (sum(node == LETTERS[1:10])!=1)
    stop("Error, node does not exist in matrix")
  
  (sum(x[node,]))
  }

#followers function
k <-0
followers<- function(k){
  if (sum(k == LETTERS[1:10])!=1)
    stop("Error, node does not exist in matrix")
  
  (sum(x[,k]))
}

#create data frame df
df <- data.frame("Person"= colnames(x), "Follows" = sapply(colnames(x),follows), "Followers" = sapply(colnames(x),followers),row.names = 1:10)

