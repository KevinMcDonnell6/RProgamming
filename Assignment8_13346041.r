#load appropriate packages
library(stringr)
library(dplyr)
library(ggplot2)

#import text file
f <- readLines("J Joyce.txt")

#create function to remove blank lines from text
#does this by finding lines that contain any letters
remove_blank_lines <- function(txt){
  txt[str_detect(txt,"[^ ]+")]
}

#apply above function on f and reset value of f
f <- remove_blank_lines(f)

#create function that makes al lower case and removes said symbols
#also seperates lines into vevtoers of single words
prepare_line <- function(txt){
  txt %>% tolower() %>% str_replace_all("(--|,|\\?|\\.|'|:|said)","") %>%
  str_extract_all("[^ ]+") 
    
}

#create single vevtor of all words
Terms <- unlist(prepare_line(f))
  
#Create tibble showing summary of all words
#group_by groups by words then frequency is taken by n()
#columns are added for pattern and word length
#then arranged by descending frequency
TermsTab <- tibble(Word=Terms) %>% group_by(Word) %>% summarise(WFrequency=n())%>%
            mutate(Pattern= paste("^",Word,"$",sep=""), 
                      WLength = str_length(Word)) %>%
            arrange(desc(WFrequency))

#create summary tibble of just words greaterthan length 3 
summ <- TermsTab %>% select(Word,WLength,WFrequency) %>%
          filter(str_length(Word)>3)

#plot word frequency vs length
ggplot(TermsTab) + geom_point(mapping = aes(x=WLength,y=WFrequency),position = "jitter")+
  xlab("Word Length") + ylab("Word Frequency")
