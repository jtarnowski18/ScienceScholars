
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

####Search for tweets using Search terms######
#This is what an improvement from cash tags would resemble. (Didn't use in paper)
#Create list of search Terms
Hot <- list(c("Ford"),
            c("GE"),
            c("\"American Airlines\""),
            c("Disney", "ABC","ESPN", "Touchstone Pictures", "Marvel", "Lucasfilm", "A&E", "Lifetime", "Pixar", "\"Hollywood Records\"", "Vice", "Core Publishing"),
            c("Delta"),
            c("Apple", "mac", "iPhone", "ipad", "Macbook", "Apple TV"),
            c("Microsoft", "Windows", "Xbox"),
            c("Carnival"),
            c("GoPro"),
            c("\"Aurora Cannabis\""),
            c("\"Plug Power\""),
            c("Tesla", "Musk", "SpaceX"),
            c("\"Norwegian Cruise Line\"", "Norwegian"),
            c("\"Bank of America\""),
            c("Boeing"),
            c("\"United Airlines\""),
            c("Fitbit"),
            c("Snapchat"),
            c("Amazon"),
            c("Uber"),
            c("\"Canopy Growth\""),
            c("\"Royal Caribbean\""),
            c("Inovio"),
            c("Twitter"),
            c("Facebook"),
            c("AMD"),
            c("Alibaba"),
            c("Cronos"),
            c("Spirit"),
            c("Moderna"),
            c("Zynga"),
            c("Coca-Cola"),
            c("NIO"),
            c("Starbucks"),
            c("Southwest", "\"Southwest Airlines\""),
            c("AT&T"),
            c("\"Marathon Oil\""),
            c("MGM"),
            c("Aphria"),
            c("\"Genius Brands\""),
            c("JetBlue"),
            c("OrganiGram"),
            c("\"MFA Financial\""),
            c("\"Exxon Mobil\""),
            c("\"United States Oil\""),
            c("Direxion"),
            c("\"ProShares Ultra\""),
            c("Netflix"),
            c("Nikola"),
            c("Invesco"),
            c("\"Virgin Galactic\""),
            c("Workhorse"),
            c("AMC"),
            c("GM", "\"General Motors\""),
            c("NVIDIA"),
            c("Nokia"),
            c("\"Vanguard S&P 500 ETF\""),
            c("\"New Residential Investment\"", "\"New Residential\""),
            c("Callon"),
            c("Draftkings"),
            c("\"Dave & Busters\""),
            c("\"Penn Nations Gaming\""),
            c("Pfizer"),
            c("\"Caralyst Pharmaceuticals\""),
            c("Tilray"),
            c("SPDR"),
            c("Sirius"),
            c("Nike"),
            c("Slack"),
            c("Berkshire"),
            c("Visa"),
            c("Square"),
            c("Lyft"),
            c("Gap"),
            c("\"Vivint Solar\""),
            c("Halliburton"),
            c("Sony", "PS5"),
            c("\"Wells Fargo\""),
            c("\"Gilead Sciences\""),
            c("\"Kosmos Energy\""),
            c("Alphabet", "Google"),
            c("\"JPMorgan Chase\"", "Chase"),
            c("\"Beyond Meat\""),
            c("PowerShares"),
            c("TherapeuticsMD"),
            c("Zoom"),
            c("\"FuelCell Energy\""),
            c("\"New York Mortgage\""),
            c("Macy's"),
            c("Occidental"),
            c("Walmart"),
            c("\"Energy Transfer\""),
            c("BP", "\"Brritish Petroleum\""),
            c("Corbus"),
            c("Sorrento"),
            c("Vaxart"),
            c("Peloton"),
            c("\"Viking Therapeutics\""),
            c("Nintendo"),
            c("Eldorado"))


Date <- Sys.Date()
#Set the original date
load("E:/Summer Fellows/Phase 2/Hot_Companies.Rdata")
  for (i in 1:nrow(Hot_Companies)) {
    #Save the file paths of where I would like to store the resulting data as variables
    #file path where I would like to store the daily data for this stocks tweets
    fileDate <- paste("E:/Summer Fellows/Phase 2/TwitterDataThree/List/",Hot_Companies$Symbols[i], Sys.Date(), ".Rdata", sep = "")
    #file path for every tweet collected for this stock
    fileFull <- paste("E:/Summer Fellows/Phase 2/TwitterDataThree/List/Full/",Hot_Companies$Symbols[i], ".Rdata", sep = "")
    if (i == 50) {
      print("I am waiting")
      Sys.time()
      Sys.sleep(900)
      print("I am running")
    } 
  
    
    #Search for new tweets
    Tweets <- search_tweets2(q = paste(Hot[[i]],"filter:verified -filter:retweets  -filter:replies", sep = " "),
                             n=10000,
                             retryOnRateLimit = TRUE,
                             parse = TRUE,
                             lang = "en"
    )
    
    Day_Split <- str_split_fixed(Tweets$created_at, "-", 3)[,3]
    Tweets$Day <- str_split_fixed(Day_Split, " ", 2)[,1]
    
    Tweets <- Tweets %>%
      filter(Day > 26 & Day<28)
    
    
    #If any tweets were downloaded
    if(nrow(Tweets) > 0 ){
      #Select only the following columns
      Tweets <- Tweets %>%
        select(user_id, status_id, created_at, screen_name,
               text, favorite_count, retweet_count, quote_count,
               reply_count, hashtags, symbols, verified)
      
      #Add a column that contains the stock symbol
      #Could be helpful when for data wrangling in the future
      Tweets$Sym <-as.character(Hot_Companies$Symbols[i])
      Tweets$Sentiment <- NA
      Tweets$BiSentiment <- NA
      #if a Full files doesn't exist for this stock, save Tweets df there
      if(file.exists(fileFull) == FALSE){
        save(Tweets, file = fileFull)
      }
      #If fileDate doesn't exist in save the file at fileDate
      #Otherwise load the fileDate and add the newly searched tweeets to it:
      if(file.exists(fileDate) == FALSE) {
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
#############################################################################

load("NewSym.Rdata")

data <- NewSym
#Go through select symbols and search tweets
for (i in 1:nrow(data)) {
  
  #Save the file paths of where I would like to store the resulting data as variables
  #file path where I would like to store the daily data for this stocks tweets
  fileDate <- paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/",data$Sym[2], Sys.Date(), ".Rdata", sep = "")
  #file path for every tweet collected for this stock
  fileFull <- paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/Full/",data$Sym[i], ".Rdata", sep = "")
  
  load(fileDate)
  
  #Search For tweets for this stock using the dollar sign index
  #I don't want to see retweets or replies
  #IF it's the first search download all the data possible
  
    if(i%%25 == 0){
      print("sleeping")
      Sys.sleep(900)
      print("Running")
      
    }
    Tweets <- search_tweets2(paste(paste("$", data$Sym[i], sep = ""),
                                   "-filter:retweets -filter:replies", sep = " "),
                             n = 10000,
                             retryOnRateLimit = TRUE,
                             parse = TRUE,
                             lang = "en")
    
    
    Day_Split <- str_split_fixed(Tweets$created_at, "-", 3)[,3]
    Tweets$Day <- str_split_fixed(Day_Split, " ", 2)[,1]
    
    Tweets <- Tweets %>%
      filter(Day >= 26 & Day<=28)
 
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
    Tweets$Sentiment <- NA
    Tweets$BiSentiment <- NA
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


# cl##############Sentiment Funcion###################
string <- Tweets$text[2]
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
LiveFiles <- list.files("E:/Summer Fellows/Phase 2/TwitterDataThree/Live")
for (i in 1:length(LiveFiles)) {
  if (grepl(as.character("2020-12-01"), LiveFiles[i], fixed = TRUE) == TRUE) {
    load(paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/",LiveFiles[i], sep = ""))
    print(paste("I:",i/length(LiveFiles)))
  
    for (j in 1:nrow(Tweets)) {
    sent_list <- Companies_Sentiment(Tweets$text[j])
    Tweets$Sentiment[j] <- sent_list[[1]]
    Tweets$BiSentiment[j] <- as.character(sent_list[[2]])
    print(paste("J:",j/nrow(Tweets)))
    
    }
  save(Tweets, file = paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/",LiveFiles[i], sep = ""))
}
  }
#For every Twitter file collected using search terms conduct a sentiment analysis
ListFiles <- list.files("E:/Summer Fellows/Phase 2/TwitterDataThree/List")

for (i in 1:length(ListFiles)) {
  if (grepl(as.character("2020-12-01"), ListFiles[i], fixed = TRUE) == TRUE) {
    load(paste("E:/Summer Fellows/Phase 2/TwitterDataThree/List/",ListFiles[i], sep = ""))
    print(paste("I:",i/length(ListFiles)))
    
    for (j in 1:nrow(Tweets)) {
      sent_list <- Companies_Sentiment(Tweets$text[j])
      Tweets$Sentiment[j] <- sent_list[[1]]
      Tweets$BiSentiment[j] <- as.character(sent_list[[2]])
      print(paste("J:",j/nrow(Tweets)))
      
    }
    save(Tweets, file = paste("E:/Summer Fellows/Phase 2/TwitterDataThree/List/",ListFiles[i], sep = ""))
  }
}









   
  