

#For data Wrangling
require(tidyverse)
#For fread
require(data.table)
#To Download Stock Prices
require(quantmod)

#Create empty vector to store stock symbols
SearchSym <- c()

#Create a function that will pull stock symbols that changed +/- four percent on the day
#Past is stock data in a long format
#Creating this extra data frame was orginally used with the intent of conncecting this process to the Tweet collecting process
Link_Twitter <- function(past){
  #For every row in the newly added stock data, if the changes absolute value is > 4
  for (j in 1:nrow(past)) {
    if(past$PerChng[j] > 4 | past$PerChng < -4){
      #Add that symbol to the list of symbols
      SearchSym <- c(SearchSym, past$Sym[j])
      print(j)
    }
    if(j == nrow(past)){
      #At the end remove duplicates
      SymTable <- as.data.frame(table(SearchSym))
      NewSym <- SymTable %>%
      filter(Freq == 1)
      # 
      if (length(NewSym$SearchSym) > 0) {
        #Create/ edit a data frame that the tweet collect can use (If made live)
        NewSym <- data.frame("Sym" = NewSym$SearchSym, "Search" = 0, "Time" = Sys.time())
        save(NewSym, file = "NewSym.Rdata") 
      }
      
    }
  }
}

#TSLA is not in Tickers for some reason
TSLA <- data.frame("Symbol" = "TSLA", "Description" = "Tesla")
#Symbol for every stock collecting data on
load("D:/Summer Fellows/tickers.Rdata")
#Add TSLA to tickers
tickers <- rbind(tickers, TSLA)
#Remove Symbols with a . or a -
tickers$Dot_Dash <- FALSE
for (i in 1:nrow(tickers)) {
  tickers$Dot_Dash[i] <- grepl("-", tickers$Symbol[i], fixed = TRUE)
  if (tickers$Dot_Dash[i] == FALSE) {
   tickers$Dot_Dash[i] <- grepl(".", tickers$Symbol[i], fixed = TRUE)
  }
  
}
rownames(tickers) <- NULL
tickers <- tickers[tickers$Dot_Dash == FALSE,]

#Remove problematic stocks, they just do not work and distrupt the program
tickers <- tickers[tickers$Symbol != "SFB",]
tickers <- tickers[tickers$Symbol != "SCA",]
tickers <- tickers[tickers$Symbol != "ACTTW",]
tickers <- tickers[tickers$Symbol != "VAM",]
tickers <- tickers[tickers$Symbol != "PYX",]
tickers <- tickers[tickers$Symbol != "SSI",]
tickers <- tickers[tickers$Symbol != "SWP",]
tickers <- tickers[tickers$Symbol != "AGFSW",]
tickers <- tickers[tickers$Symbol != "APY",]
tickers <- tickers[tickers$Symbol != "AGN",]
tickers <- tickers[tickers$Symbol != "AVH",]
tickers <- tickers[tickers$Symbol != "DXB",]
tickers <- tickers[tickers$Symbol != "EQM",]
tickers <- tickers[tickers$Symbol != "ERA",]
tickers <- tickers[tickers$Symbol != "FG",]
tickers <- tickers[tickers$Symbol != "HJV",]
tickers <- tickers[tickers$Symbol != "I",]
tickers <- tickers[tickers$Symbol != "JBN",]
tickers <- tickers[tickers$Symbol != "JCP",]
tickers <- tickers[tickers$Symbol != "KEM",]
tickers <- tickers[tickers$Symbol != "KTP",]
tickers <- tickers[tickers$Symbol != "LTM",]
tickers <- tickers[tickers$Symbol != "MNE",]
tickers <- tickers[tickers$Symbol != "PIY",]
tickers <- tickers[tickers$Symbol != "PYX",]
tickers <- tickers[tickers$Symbol != "SBNA",]
tickers <- tickers[tickers$Symbol != "SRF",]
tickers <- tickers[tickers$Symbol != "TSLF",]
tickers <- tickers[tickers$Symbol != "UNT",]
tickers <- tickers[tickers$Symbol != "WBC",]


#Set condition for a whole loop
Run <- TRUE
#Tracker for loop count
i <- 1

#Fuction to download stock prices
QuoteScrapper <- function(x) {
  #If the past df doesn't exist it, make it exist
if (exists("past") == FALSE) {
  past <- data.frame()
}

#Run until I say otherwise
while (Run == TRUE) {
#Download all the stock prices for the symbols in tickers through yahoo
current <-getQuote(tickers$Symbol , src = "yahoo")
#Make the row names a column (The function makes the symbol a row name)
setDT(current,keep.rownames = TRUE)
#Rename the columns for convenience
names(current)[1] <- "Sym"
names(current)[2] <- "Time"
names(current)[3] <- "Price"
names(current)[5]<- 'PerChng'
#See Link twitter function at top of script
Link_Twitter(current)
#Update the stock data file for this day
past <- rbind(past, current)
#Save the Stock data as a Rdata file using the date as its name
  save(past, file = paste("D:/Summer Fellows/Phase 2/Intraday/",Sys.Date(),".Rdata"))

print(Sys.time())
#Pause for 2 minutes
Sys.sleep(120)
}
}

QuoteScrapper(TRUE)
