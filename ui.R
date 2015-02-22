library(shiny)

data <- read.csv("data.csv", header = TRUE, sep = ",")
data <- na.omit(data)
#length(data[,1])
#head(data)
#str(data)

teamName<-sort(unique(data[,2]))
d<-as.vector.factor(teamName)

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).

# Define the overall UI
shinyUI(
    
    # Use a fluid Bootstrap layout
    fluidPage(    
        
        # Give the page a title
        titlePanel("Cricket World cup 2015 - Team Analysis"),
        br(), 
        p('This app analyses the batting performance of a team and provide 
                a fair idea on which team has a higher chances of wiining.'),        
        br(),br(),
        # Generate a row with a sidebar
        sidebarLayout(      
            
            # Define the sidebar with one input
            sidebarPanel(
                selectInput("teamA", "TEAM A:", d,
                            selected=d[[2]]),
                hr(),
                selectInput("teamB", "TEAM B:", d,
                            selected=d[[6]]),
                br(),br(),br(),
                helpText("All Data taken from ESPNcricinfo.com ")
            ),
            
            # Create a spot for the barplot
            mainPanel(
                
                tabsetPanel(type = "tabs", 
                            tabPanel("TEAM - A Runs", plotOutput("plot")),
                            tabPanel("A vs. B (Total Runs)", plotOutput("plotTotal")),
                            tabPanel("A vs. B (Bucket Comparison)", plotOutput("plotBucket")),
                            tabPanel("Summary", verbatimTextOutput("summary")), 
                            tabPanel("Table", tableOutput("table"))
                )
            )
        )
    )
)