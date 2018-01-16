#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

file <- read.csv("logs/telemetryLog-1969.12.31.20.51.15.csv", header=TRUE)
file <- file[c(1,which(rowSums(diff(as.matrix(file[,grep('time',names(file))])))!=0)+1),]
logNames <- names(file)
logTimes <- file[-1,1]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # get data based on input$dataVal from ui.R
    
    xData <- logTimes
    yData <- file[-1, input$dataVal]
    name <- input$dataVal
    curData <- data.frame(x, y)
    
    
    # draw plot with specified data values
    if (input$plotType == "smooth") ggplot(curData, aes(x=xData, y=yData)) + geom_point() + geom_smooth(method = "loess") + labs(x = "Time", y = name)
    else (ggplot(curData, aes(x=xData, y=yData)) + geom_line() + labs(x = "Time", y = name))
    #qplot(data=curData)
    
  })
  
})
