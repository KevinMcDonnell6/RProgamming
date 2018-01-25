

#create timer function which is a list of functions within a function
#This creates an enviroment within which we can store our variables
timer<- function(){
  #Reset all the variables
  i<-NA
  f<-NA
  s<-NA
  #I've created a variable New which I use to make sure I am only seeing the most current values 
  New<-NA
  
  #create the list of functions
  list(
    
    #Start function resets all variables so stored values are lost
    #also sets i to start time (current time)
    Start=function(){ i<<- Sys.time()
                         New<<- T     #New set to T, but could be anything except NA
                         f<<- NA
                         s<<- NA},
       
    #Stop function is an if else statement that checks if we have a current run (checks New)
    Stop=function(){ if(is.na(New)){stop("Error, Cannot stop as timer was not started")} 
      
                          #if its a current run it sets f to the current time - start time
                         else{  f<<- difftime(Sys.time(),i,units = "secs"); New<<-NA}},
       
    #Split function also checks to see if currently running by New   
    Split=function(){ if(is.na(New)){stop("Error, Cannot split as timer was not started")}
      
                          #if current run, sets s to start time - current time
                         else { s<<-difftime(Sys.time(),i,units = "secs")}},
    
    #Get split returns split if there is one
    get_split=function(){if(is.na(s)){stop("Error, Cannot get split as split was not called")}
                            else{print(s)}},
    
    #Get time returns the time if Stop function has being called   
    get_time=function(){ if(is.na(f)){stop("Error, Cannot get time as stop was not called")}
                            else {print(f)}},
       
    #get state returns values of start time, fiinish time and most recent split called
    get_state=function() list(Start=i,Finish=f,Split=s)
                      
       
  )
}

#assign timer to t so functions can be called
t<-timer()



