
#Create function to contain our timer functions
#this creates anenviroment where we can store our timer variables
timer<- function(){
  
     #create list to store our timer variables 
     State <- list(init=Sys.time(),Start=NA,Finish=NA,Split=NA)
     
     #create list of functions within timer
     list(
       
          #start function assigns assigns value to start time and resets finish and split
          Start=function(){ State$Start <<-Sys.time()
                            State$Finish<<-NA
                            State$Split<<- NA},
          
          #stop function checks for NA (if timer has started) and returns error staatement if true
          #assigns finish value if false
          Stop=function(){if( is.na(State$Start)){
                                  stop(" Error, Cannot stop as timer was not started.",call. = F)}
                                  else{invisible(State$Finish <<- Sys.time())}},
          
          #split function checks for NA (if timer has started) and returns error staatement if true
          #assigns split value if false
          Split=function(){ if(is.na(State$Start)){
                                   stop("Error, Cannot split as timer was not started.",call. = F)}
                                       else{ State$Split <<- Sys.time() }},
          
          #get split checks if split has been called and returns and error if it hasn't
          #returns split time if it has been called
          get_split=function(){ifelse(is.na(State$Split),
                                      stop("Error, Cannot get split as split was not called.", call. = F),
                                       return(difftime(State$Split,State$Start,units = "secs"))
                                      )
                              },
                                                      
                                
          #get time checks if stop has been called and returns and error if it hasn't
          #returns finish time if it has been called
          get_time=function(){ifelse(is.na(State$Finish),
                                     stop("Error, Cannot get time as stop was not called.", call. = F),
                                     return(difftime(State$Finish,State$Start,units = "secs"))
                                     )
                              },
          
          #get state funtion returns a list ofthe times all functions were called
          get_state=function() list(Init=State$init,Start=State$Start,Finish=State$Finish,Split=State$Split)
                         
          
     )
   }

#assign timer() to t so fnctions caan be called
t<-timer()

#check structure of get state
str(t$get_state())

#test error codes on split and stop
t$Split()
t$Stop()

#start timer
t$Start()

#test error codes on get split and get time
t$get_split()
t$get_time()

#take the split time andstop time
t$Split()
t$Stop()

#use get split and get time
t$get_split()
t$get_time()

#check get state and its structure
t$get_state()
str(t$get_state())

