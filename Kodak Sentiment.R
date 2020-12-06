require(ggplot2)
require(stringr)
require(tidyverse)


#Sentiment Analysis Function
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
Tweets$Sentiment <-NA
#Run all of the Kodak Tweets through the sentiment analysis function
for (j in 1:nrow(Tweets)) {
  sent_list <- Companies_Sentiment(Tweets$text[j])
  Tweets$Sentiment[j] <- sent_list[[1]]
  if (j%%50 == 0) {
    print(j)
  }
  
}


#Scatter Plot of Kodaks sentiments
Sentiments <- ggplot(Tweets,aes(created_at, Sentiment))+
  geom_point()+
  ggtitle("Kodak's Sentiment Scores")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Date")


#Kodaks Stock Price Plot
Beg <- as.date("2020-07-27")
beg <- as.Date.character("2020-07-27")
Aft <- as.Date.character("2020-08-05")
Price <- ggplot(KODK_Data, aes(Date, High))+
  geom_line()+
  geom_vline(xintercept = beg, col = "red")+
  geom_vline(xintercept = Aft, col = "red")+
  ggtitle("Kodak's Stock Price")+
  theme(plot.title = element_text(hjust = 0.5))
  



ggsave("E:/Summer Fellows/Phase 2/Kodak_Plot.png", Sentiments, "png")

save(Tweets, file = "KODK.Rdata")