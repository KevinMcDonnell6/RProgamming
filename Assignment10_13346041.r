

#create function to change class
my_lm_create <- function(linmod){
                
                class(linmod) <- c("my_lm", "lm")
                linmod
}

#create new linear model
l <- my_lm_create(lm(eruptions~waiting,data=faithful))


#create new summary function
summary.my_lm <- function(linmod){
  
                  #create line
                  cat("=======================================================\n")
      
                  #print line of text
                  cat("My Summary Function for the Linear Model Class my_lm\n\n")
                  
                  #print the coeeficients
                  cat("Coefficients\n")
                  print(linmod$coefficients)
                  
                  #print the residuals
                  cat("\nResiduals\n")
                  print(summary(linmod$residuals))
                  
                  #print the number of observations of the first variable
                  cat("\nNumber of observations for",colnames(linmod$model[1]),"is",
                      length(linmod$model[,1]))
                  
                  ##print the number of observations of the second variable
                  cat("\nNumber of observations for",colnames(linmod$model[2]),"is",
                      length(linmod$model[,2]))
                  
                  #print the R-squared and adjusted R-squared values
                  cat("\n\nR Squared Value",summary.lm(linmod)$r.squared)
                  cat("\nAdjusted R Squared Value",summary.lm(linmod)$adj.r.squared)
                  
                  cat("\n=======================================================")
}

#revert model back to lm
class(l) <- "lm"
summary(l)
