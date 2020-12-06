
#Edit Strings
require(stringr)
#Data Wrangling
require(tidyverse)

#Load in the stock data
load("E:/Summer Fellows/Phase 2/Intraday/2020-11-27 .Rdata")
#All of the Twitter Data files I have
LiveFiles <- list.files("E:/Summer Fellows/Phase 2/TwitterDataThree/Live")
#Create a vector that stores stock symbols that don't have any tweets but have a file
Trash <- c()
#For every differnt twitter data file
for (i in 1:length(LiveFiles)) {
  #Make sure file exists (More for when More than 1 day of Twitter Data exists)
  if (grepl(as.character("2020-12-01"), LiveFiles[i], fixed = TRUE) == TRUE) {
    #Load in the Twitter Data
    load(paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/",LiveFiles[i], sep = ""))
#Change the time to ETC instead of UTC (Left in separate vectors for convenience)
Tweets_Time_Split <- str_split_fixed(Tweets$created_at, "-", 3)
Tweets_Time_Split <- str_split_fixed(Tweets_Time_Split[,3], " ", 2)
Tweets$Day <- as.numeric(Tweets_Time_Split[,1])
Tweets_Time <- str_split_fixed(Tweets_Time_Split[,2], ":", 3)
Tweets$Hour <- as.numeric(Tweets_Time[,1])
Tweets$Minute <- as.numeric(Tweets_Time[,2])
Tweets$E_Hour <- Tweets$Hour - 5

#Fix the the Day column after subtracting five hours (No negative time just a previous day)
for (j in 1:nrow(Tweets)) {
  if (Tweets$E_Hour[j] < 0) {
    Tweets$Day[j] <- Tweets$Day - 1
    Tweets$E_Hour[j] <- 24 + Tweets$E_Hour[j]
    
  }
}

#Filter out dates again, after changing time zones
Tweets <- Tweets %>%
  filter(Day >= 26 & Day <= 28)
#If there are still Tweets Left
if (nrow(Tweets) > 0 ) {
#This is measure of time in minutes from the beginned of 11/26/2020 (Much easier to work with than dates)
Tweets$Three_Day <- NA
#For every tweet in the data frame
for(j in 1:nrow(Tweets)){
  #If its the 26th find how many mintues have expired since the beginning of the day
  if(Tweets$Day[j] == 26){
    Tweets$Three_Day[j] <- Tweets$E_Hour[j]*60 + Tweets$Minute[j]
  }
  #Find how many Minutes have expired from the begging of the 26th
  if(Tweets$Day[j] == 27){
    Tweets$Three_Day[j] <- 1440 + Tweets$E_Hour[j]*60 + Tweets$Minute[j]
  }
  if(Tweets$Day[j] == 28){
    Tweets$Three_Day[j] <- 2880 + Tweets$E_Hour[j]*60 + Tweets$Minute[j]
  }
}
}else{
  #Update Trash vector
  Trash <- c(Trash, i)
}
#Update the Data Frame
save(Tweets , file = paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/",LiveFiles[i], sep = ""))
  }
}

#Take out the trash Data Frame
#Commented out because if edited could ruin the data
# for (k in 1:16) {
#   toss <- Trash[k]
#   file.remove(paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/",LiveFiles[], sep = ""))
# }

#Pull the symbols of the file names
Symbols <- str_split_fixed(LiveFiles, "2020", 2)[,1]

#Take only the stocks that are in Symbols from the Stock data
Select_Stocks <- data.frame()
for (i in 1:length(Symbols)) {
  Stocks <- past%>%
    filter(past$Sym == Symbols[i])
  Select_Stocks <- rbind(Select_Stocks, Stocks)
  print(i)
}


#Turn the Date into Vectors
Date <- str_split_fixed(Select_Stocks$Time, " ", 2)[,1]
Time <- str_split_fixed(Select_Stocks$Time, " ", 2)[,2]

Select_Stocks$Day <- as.numeric(str_split_fixed(Date, "-", 3)[,3])
Select_Stocks$Hour <- as.numeric(str_split_fixed(Time, ":", 3)[,1])
Select_Stocks$Minute <- as.numeric(str_split_fixed(Time, ":", 3)[,2])

#Filter out any data thats not from the 27th
Select_Stocks <- Select_Stocks%>%
  filter(Day == 27)


#Make a vector that will match up with the Three_Day vector in the Twitter Data for the 27th
for (i in 1:nrow(Select_Stocks)) {
Select_Stocks$Time_Overlap[i] <- 1440 + Select_Stocks$Hour[i]*60 + Select_Stocks$Minute[i]

}
#Create a vector with T/F if the absolute value of the percent changed is over 4%
Select_Stocks$Four_Change <- NA
  for (j in 1:nrow(Select_Stocks)) {
    if (Select_Stocks$PerChng[j] > 4 | Select_Stocks$PerChng[j] < -4) {
      Select_Stocks$Four_Change <- TRUE
    }
    
}
#Then filter out only the TRUE
Select_Stocks <- Select_Stocks%>%
  filter(Four_Change == TRUE)
#All of the stocks changed +/-4% at the open

#Create a vector that states whether the tweet happend before or after the % change
for (i in 1:length(LiveFiles)) {
  if (grepl(as.character("2020-12-01"), LiveFiles[i], fixed = TRUE) == TRUE) {
    load(paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/",LiveFiles[i], sep = ""))
    Tweets$B_A <- "B"
    save(Tweets , file = paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/",LiveFiles[i], sep = ""))
  }
}

for (i in 1:length(LiveFiles)) {
  if (grepl(as.character("2020-12-01"), LiveFiles[i], fixed = TRUE) == TRUE) {
    load(paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/",LiveFiles[i], sep = ""))
    for (j in 1:nrow(Tweets)) {
      if (Tweets$Three_Day[j] > 2010) {
        Tweets$B_A[j] <- "A"
    }
    
    }
    save(Tweets , file = paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/",LiveFiles[i], sep = ""))
  }
}

#Summary Frame
Final_Frame <- data.frame("Symbol" = Symbols,
                          "Sentiment_Before" = 0,
                          "Sentiment_Before_None" = 0,
                          "Sentiment_After" = 0,
                          "Sentiment_After_None" = 0,
                          "Non_Zero_Before" = 0,
                          "Non_Zero_After" = 0)


for (i in 1:length(LiveFiles)) {
  if (grepl(as.character("2020-12-01"), LiveFiles[i], fixed = TRUE) == TRUE) {
    load(paste("E:/Summer Fellows/Phase 2/TwitterDataThree/Live/",LiveFiles[10], sep = ""))
    #Avg Tweet Sentiment before
    Final_Frame$Sentiment_Before[i] <- mean(Tweets$Sentiment[Tweets$B_A == "B"], na.rm = TRUE)
    #Avg Tweet Sentiment before (Without Zeros)
    Final_Frame$Sentiment_Before_None[i] <- mean(Tweets$Sentiment[Tweets$B_A == "B" & Tweets$Sentiment != 0])
    #Avg Tweet Sentiment after 
    Final_Frame$Sentiment_After[i] <- mean(Tweets$Sentiment[Tweets$B_A == "A"])
    #Avg Tweet Sentiment after (Without Zeros)
    Final_Frame$Sentiment_After_None[i] <- mean(Tweets$Sentiment[Tweets$B_A == "A" & Tweets$Sentiment != 0])
    #Percent of nonzero sentiments before
    Final_Frame$Non_Zero_Before[i] <- length(Tweets$Sentiment[Tweets$B_A == "B" & Tweets$Sentiment != 0])/nrow(Tweets)
    #Percent of nonzero sentiments after
    Final_Frame$Non_Zero_After[i] <- length(Tweets$Sentiment[Tweets$B_A == "A" & Tweets$Sentiment != 0])/nrow(Tweets)
  }
  #Save File
    save(Final_Frame, file = "E:/Summer Fellows/Phase 2/SummaryFrame.Rdata")
  }

