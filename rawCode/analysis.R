# read data from the csv file
data <- read.csv("data.csv", header = TRUE, sep = ",")
data <- na.omit(data)
length(data[,1])
head(data)
str(data)

# fix two variables - teamA, teamB
teamA <- "India"
teamB <- "Sri Lanka"

dataA <- data[ which(data$Home.Country == teamA 
                       & data$Opposite.Country == teamB), ]

dataB <- data[ which(data$Home.Country == teamB 
                     & data$Opposite.Country == teamA), ]

# collect runs scored by runsA,totalRunsA, totalFoursA, totalSixA
runsA <- dataA$runs
totalRunsA <- sum(runsA)
totalFoursA <- sum(dataA$Fours)
totalSixA <- sum(dataA$Six)

# collect runs scored by runsB,totalRunsB, totalFoursB, totalSixB
runsB <- dataB$runs
totalRunsB <- sum(runsB)
totalFoursB <- sum(dataB$Fours)
totalSixB <- sum(dataB$Six)


# plot the graphs of totalRuns
slices <- c(totalRunsA, totalRunsB)
teamName <- c(teamA,teamB)
percentage <- round(slices/sum(slices)*100)
sliceName <- paste(teamName," ", percentage, "%", sep="")

subLabel <- paste("Total Runs" , totalRunsA, "from feb 2014 - feb 2015")

pie(slices,labels = sliceName, 
    col = c("red","blue"), 
    main = "Total runs socred against each other", sub=subLabel)

# plot the density of runs scored by each team (3 Histogram)
densityA <- density(runsA)
densityB <- density(runsB)
yrange <- range(densityA$y,densityB$y)
plot(densityA, col="red", main="Density of runs", lwd=2, ylim=yrange)
lines(densityB, col="blue")

#creating buckets for A
countRunsA30 <- length(runsA[ which( runsA > 0 & runsA < 30)])
countRunsA50 <- length(runsA[ which( runsA > 31 & runsA < 50)])
countRunsA100 <- length(runsA[ which( runsA > 51 & runsA < 100)])
countRunsA100Plus <- length(runsA[ which( runsA > 101)])

countRunsB30 <- length(runsB[ which( runsB > 0 & runsB < 30)])
countRunsB50 <- length(runsB[ which( runsB > 31 & runsB < 50)])
countRunsB100 <- length(runsB[ which( runsB > 51 & runsB < 100)])
countRunsB100Plus <- length(runsB[ which( runsB > 101)])

runsBucketsA =  c(countRunsA30, 
                   countRunsA50,
                   countRunsA100,
                   countRunsA100Plus)

runsBucketsB =  c(countRunsB30, 
                   countRunsB50,
                   countRunsB100,
                   countRunsB100Plus)

runsBucketsAB = rbind(runsBucketsB,runsBucketsA)
colBuckets = c("0-30","30-50", "50-100", "100+")

barplot(runsBucketsAB, main="Frequency of Runs into four groups in Y Axes", beside=TRUE, horiz=TRUE,
        names.arg=colBuckets, col=c("blue","red"),
        xlab=" Frequency in the group  ", ylab="Runs - 4 Buckets")

