
#Load in packages
#Search Twitter with R:
library(rtweet)
#Needed to link Twitter and R:
library(httpuv)
#Needed for data wrangling:
library(tidyverse)
#More data wrangling:
require(data.table)
#Working with strings:
require(stringr)
#
require(tidytext)
require(textdata)
require(textclean)

require(SentimentAnalysis)
require(qdapRegex)

#This is what Live Tweet collecting would look Like
############## App Set-Up #################### 

app_name = "JacobTarnowskiAppForR"
consumer_key = "dKMeeCwg8mo55CLQQoDp6t4Gs"
consumer_secret = "j2VHkbIdSpomDsmSAV0gdPT2bckKSc1PCLYsVhxy2Q3ZGNYEwm"
access_token = "1059118494179041281-pWTSpU85ha4klVlkw1vsz8T0zSlBD6"
access_secret = "hn7yan92YlUyUoiYuSfkjBidQ9dVecRYdF1H4CHH5MaCt"

twitter_token <-create_token(app = app_name,
                             consumer_key = consumer_key,
                             consumer_secret = consumer_secret,
                             access_token = access_token,
                             access_secret = access_secret)


################Basic Frame###############
#Run is a condtion variable that will be true until the program tell us it isn't:
run = TRUE

#While run is true search fro the this data frame in the working directory until it appears.
#Take a three minute break between attempts
while (run == TRUE) {
  
#If the file exists enter the Search_Companies function:
#Search_Companies will run until the time specified:
#After search companies is over run should equal False
if (file.exists("NewSym.RData") == TRUE) {
Search_Companies(0)
    run = FALSE
} else{
  print("Sleeping")
  #If the file doesn't exist take a three minute break:
    Sys.sleep(180)
  }
  
}


################## Search Companies function ##################
#The Search Companies function will search for tweets on the Companies the Quote Collector specifies
#This is done by using the dollar sign indexing tool used on twitter to look for financial mentions (Ex. $AAPL)
#I understand this is not going to give me a fully correct picture of what is going on with a company nor
#return the Maximum amount of tweets about the company. To get a true picture of a consumers view of a
#company on twitter I would need to develop a lexicon of search terms for each of the stocks I have data on.
#While I understand this is achieveable, that was not the goal of this project, but will be worked on in future work.

Search_Companies <- function(a){
a <- a+2
mydatetime <- as.POSIXct("2018-01-01 23:00:00", tz = "EST")
#Load in the Data from the Quote Collector:
#This is the Symbols that are up or down more than 4% on that given day
  
#The data frame loaded contains there columns, one with the stock symbols,
#one with the Search number, and one with the time it was added to the list.
#Each column is explained below.
  
#I need the stock symbol to search for tweets.
  
#I Need the search number because the first search for the companies $ index is going to be different...
#from  every search after that. Hence I need something to tell me what search I am on  
  
#I need the time just because I thought the time the stock changed more than four percent would be...
#A nice thing to have
load("NewSym.Rdata")

#t is going to be the number that overall number of times the following for loop has been performed:
 t <- 1

#Copy the NewSym df and name it data
 data <- NewSym
 
#Run until 6 p.m.
 
 while (format(Sys.time(), "%H:%M:%S", usetz = T) < format(mydatetime, "%H:%M:%S", usetz = T)) {
   
   #Load in new Symbols add them and their additional information to the data data frame
   #There will only be new symbols if t > 1
   if (t != 1) {
     load("NewSym.Rdata")
     data <- rbind(data, NewSym)
   }
   data$Sym <-as.character(data$Sym)
   #Go through select symbols and search tweets
   for (i in 1:nrow(data)) {
     
     #Save the file paths of where I would like to store the resulting data as variables
     #file path where I would like to store the daily data for this stocks tweets
     fileDate <- paste("D:/Summer Fellows/Phase 2/TwitterDataTwo/",data$Sym[i], Sys.Date(), ".Rdata", sep = "")
     #file path for every tweet collected for this stock
     fileFull <- paste("D:/Summer Fellows/Phase 2/TwitterDataTwo/Full/",data$Sym[i], ".Rdata", sep = "")
     
     
     
#Search For tweets for this stock using the dollar sign index
#I don't want to see retweets or replies
#IF it's the first search download all the data possible
    if (data$Search[i] == 0) {
      if(i%%25 == 0){
        print("sleeping")
        Sys.sleep(900)
        print("Running")
        
      }
  Tweets <- search_tweets2(paste(paste("$", "AAPL", sep = ""),
                                 "-filter:retweets -filter:replies", sep = " "),
                           retryOnRateLimit = TRUE,
                           parse = TRUE,
                           lang = "en")
} else {
  if(i%%50 == 0){
    print("sleeping")
    Sys.sleep(900)
    print("Running")
    
  }
#If it isn't the first searchd ownload the 200 most recent (or if less is available, every tweet)
Tweets <- search_tweets2(paste(paste("$", data$Sym[i], sep = ""),
                                    "-filter:retweets -filter:replies", sep = " "),
                              retryOnRateLimit = 200,
                              parse = TRUE,
                            lang = "en")
print(data$Sym[i])
}
#If any tweets were downloaded
if(nrow(Tweets) > 0 ){
  #Select only the following columns
     Tweets <- Tweets %>%
       select(user_id, status_id, created_at, screen_name,
              text, favorite_count, retweet_count, quote_count,
              reply_count, hashtags, symbols, verified)
     
  #Add a column that contains the stock symbol
  #Could be helpful when for data wrangling in the future
     Tweets$Sym <-data$Sym[i]
  #if a Full files doesn't exist for this stock, save Tweets df there
     if(file.exists(fileFull) == FALSE){
       save(Tweets, file = fileFull)
     }
  #If fileDate doesn't exist in save the file at fileDate
  #Otherwise load the fileDate and add the newly searched tweeets to it:
    if( file.exists(fileDate) == FALSE) {
    save(Tweets, file = fileDate)
    } else {
      Tweets_new <- Tweets
      load(fileDate)
      Tweets <- rbind(Tweets, Tweets_new)
      Tweets <- Tweets[Tweets$status_id == unique(Tweets$status_id),]
      save(Tweets, file = fileDate)
      
    }
}
   }
t <- 1+t
data$Search <- data$Search+1
   
 }
}
 
string <- Tweets$text[1]
string


###############Sentiment Funcion###################

 Companies_Sentiment <- function(string){


     string <- replace_emoji(string)
     #Split the string at each space
     string_sep <- as.data.frame(str_split(string = string, " "))
     #change the name of the resulting column to tweet
     names(string_sep)[1] <- "tweet"
     #Make sure each observation is a string not a factor
     string_sep$tweet <- as.character(string_sep$tweet)
     #create a space to store row numbers that contain links (to be removed)
     links <- c()
     emoji <- c()
     #For each word in the string
     for (z in 1:nrow(string_sep)) {
      # Take out the numbers
       string_sep$tweet[z] <- gsub('[0-9]+', '', string_sep$tweet[z])
      # Take out the punctuation
       string_sep$tweet[z] <-
         str_replace_all(string_sep$tweet[z] , "[[:punct:]]", "")
      # If there is a link marke the row number (will be removed after loop to avoid messing with the loop)
       if (grepl("http", string_sep$tweet[z], fixed = TRUE) == TRUE) {
         links <- c(links, z)
       }
       if (grepl("<", string_sep$tweet[z], fixed = TRUE) == TRUE) {
         emoji <- c(emoji, z)
       }

     }
     #Remove links and emoji
     remove <- unique(c(emoji,links))

     if (length(remove) > 1) {
       string_sep <-string_sep[-remove,]
     }

     string_sep <- paste(string_sep, collapse = " ")
     sent <- analyzeSentiment(string_sep)
     sent_binary <- convertToBinaryResponse(sent$SentimentQDAP)
     sent_list <- list(sent$SentimentQDAP, sent_binary)
      return(sent_list)

 }

#For every Twitter data file collected using cashtags conduct a sentiment analysis
LiveFiles <- list.files("E:/Summer Fellows/Phase 2/TwitterDataTwo/")
for (i in 1:length(LiveFiles)) {
  if (grepl(as.character("2020-12-01"), LiveFiles[i], fixed = TRUE) == TRUE) {
    load(paste("E:/Summer Fellows/Phase 2/TwitterDataTwo/",LiveFiles[i], sep = ""))
    print(paste("I:",i/length(LiveFiles)))
    
    for (j in 1:nrow(Tweets)) {
      sent_list <- Companies_Sentiment(Tweets$text[j])
      Tweets$Sentiment[j] <- sent_list[[1]]
      Tweets$BiSentiment[j] <- as.character(sent_list[[2]])
      print(paste("J:",j/nrow(Tweets)))
      
    }
  }
  save(Tweets, file = paste("E:/Summer Fellows/Phase 2/TwitterDataTwo/",LiveFiles[i], sep = ""))
  }
