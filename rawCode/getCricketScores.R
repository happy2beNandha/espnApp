seperatePlayer_Country <- function(rawPlayer) {
    temp <- strsplit(rawPlayer, " ")    
    
    last <- length(temp[[1]])
    country <- temp[[1]][last]
    
    playerArray <- setdiff(temp[[1]],temp[[1]][last])
    playerName <- paste(playerArray, collapse=" ")
    
    cleanedData <- list("playerName" = playerName, "country" = country)
}


# reading data from the webpage

library(XML)

webPageURL = "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=2;spanmin1=21+Feb+2014;spanval1=span;template=results;type=batting;view=innings"
htmlTags = readHTMLTable(webPageURL,stringsAsFactors = FALSE)
htmlData  = htmlTags$"Innings by innings list"

totalPages = 62

for ( i in 2:totalPages) {
    updatedURL = paste(webPageURL,";page=",i,sep="")
    htmlTags = readHTMLTable(updatedURL,stringsAsFactors = FALSE)
    temp = htmlTags$"Innings by innings list"
    htmlData<-rbind(htmlData,temp)
    print(temp[1,1])
}

# viewing the data received from the portal
head(htmlData)

#Taking only the columns to be used in the project
rawData = cbind("Name&Country" = htmlData[,1], "Opposite" = htmlData[,10],
                "Runs" = htmlData[,2], "4s" = htmlData[,5], 
                "6s" = htmlData[,6])
head(rawData)
dubRawData <- rawData;

#removing NA from the dataset
rawData <- na.omit(rawData)
totalRow <- length(rawData[,1])

# creating two columns for splitting the strings
rawPlayer <- rawData[,1]
cleanedPlayer <- lapply(rawPlayer, seperatePlayer_Country)

#creating an array to store the cleaned data
playerName <- array(dim=c(totalRow,1))
homeCountry <- array(dim=c(totalRow,1))

for ( i in 1:totalRow) {
    playerName[i] = cleanedPlayer[[i]]$playerName
    homeCountry[i] = cleanedPlayer[[i]]$country
}

#clean the opposition team data ie removing "v " so starting with 3rd character
rawOpposite <- rawData[,2]
oppositeCountry <- c<-substr(rawOpposite,3,nchar(rawOpposite))

uniqueAbb <- sort(unique(homeCountry))
totalUniqueAbb <- length(uniqueAbb)

#Change the unique thing based on expansion
print(uniqueAbb)
uniqueExp <- c("Afghanistan", "Australia", "Bangladesh", "England", 
               "Honk Kong", "India", "Ireland", "New Zealand", "Pakistan", 
               "Papua New Guinea", "South Africa", "Scotland",
               "Sri Lanka", "U.A.E", "West Indies", "Zimbabwe")

print(uniqueExp)
for ( i in 1:totalUniqueAbb) {
    temp <- grep(uniqueAbb[i],homeCountry)
    homeCountry[temp] = uniqueExp[i]
}

# removing the * symbol in the runs data and storing storing them as strings
rawRuns <- rawData[,3]
runsScored <- gsub("[[:punct:]]", "", rawRuns)
runsScored <- as.numeric(runsScored)

# storing the 4's data and convertime them to numeric  
fours <- rawData[,4]
fours <- as.numeric(fours)

# storing the 6's data and convertime them to numeric  
six <- rawData[,5]
six <- as.numeric(six)

cleanedData <- data.frame("Player Name" = playerName, 
                          "Home Country" = homeCountry,
                          "Opposite Country" = oppositeCountry,
                          "runs" = runsScored,
                          "Fours" = fours,
                          "Six" = six )

fileName <- "data.csv"
if (file.exists(fileName)) file.remove(fileName)

write.csv(cleanedData, file = fileName, row.names=FALSE)

#matching unique contries

"(SL)" = "Sri Lanka"
"(Ban)" = "Bangladesh"
"(WI)" = "West Indies"
"(Afg)" = "Afghanistan"
"(Aus)" = "Australia"
"(India)" = "India"
"(Zim)" = "Zimbabwe"
"(SA)" = "South Africa"
"(Eng)" = "England"
"(UAE)" = "U.A.E"
"(Pak)" = "Pakistan"
"(NZ)" = "New Zeland"
"(Ire)" = "Ireland"
"(HK)" = "Hong kong"
"(Scot)" = "Scotland"
"(PNG)" = "Papua New Guinea"