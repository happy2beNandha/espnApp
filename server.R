cricketData <- read.csv("data.csv", header = TRUE, sep = ",")
cricketData <- na.omit(cricketData)
teamName<-sort(unique(cricketData[,2]))
d<-as.vector.factor(teamName)
teamName = "Australia"
getRuns<-function(teamName) {
    cricketData <- read.csv("data.csv", header = TRUE, sep = ",")
    cricketData <- na.omit(cricketData)
    teamData <- cricketData[ which(cricketData$Home.Country == teamName),]
    teamData$runs
}

shinyServer(function(input, output, session) {
    
    # Combine the selected variables into a new data frame
        
    homeTeam <- reactive({input$teamA })
              
    output$text1 <- renderPrint({        
        input$teamA
    })
    
    output$plot <- renderPlot({
        runs <- getRuns(input$teamA)                  
        mainLabel <- paste("Histogram of Runs Scored by ", input$teamA)
        subLabel <- paste("One Day Internationals, from Feb 2014 - Feb 2015")
        hist(runs, main=mainLabel, sub= subLabel,
             xlab=" RUNS ", ylab="Frequency of Runs")             
    })
    
    output$plotTotal <- renderPlot({
        
        runsA <- getRuns(input$teamA)
        runsB <- getRuns(input$teamB)
        totalRunsA <- sum(runsA)
        totalRunsB <- sum(runsB)
        slices <- c(totalRunsA, totalRunsB)
        teamName <- c(input$teamA,input$teamB)
        percentage <- round(slices/sum(slices)*100)
        sliceName <- paste(teamName," ", percentage, "%", sep="")
        
        subLabel <- paste("Total Runs" , totalRunsA, "from feb 2014 - feb 2015")
        
        pie(slices,labels = sliceName, 
            col = c("red","blue"), 
            main = "Total runs socred against each other",
            sub = subLabel)
        
    })
    
    output$plotBucket <- renderPlot({
        
        runsA <- getRuns(input$teamA)
        runsB <- getRuns(input$teamB)
        
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
    })
    
    output$summary <- renderPrint({
        summary(cricketData)
    })        
    
    output$table <- renderTable({
        data.frame(cricketData)
    })
})