#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


smoothDerivative <- function(x, y){
  spl <- smooth.spline(x, y)
  pred.prime <- predict(spl, deriv=1)
  return(pred.prime$y)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    file <- read.csv(paste("logs/",input$fileName, sep=""), header=TRUE)
    file <- file[c(1,which(rowSums(diff(as.matrix(file[,grep('time',names(file))])))!=0)+1),]
    logNames <- names(file)
    logTimes <- file[-1,1]
    
    #Estimate acceleration based on velocity
    
    leftAccel <- smoothDerivative(file$time, file$left.velocity)
    rightAccel <- smoothDerivative(file$time, file$right.velocity)
    lAdjusted <- file[leftAccel < 0.2]
    rAdjusted <- file[rightAccel < 0.2]
    
    #Estimate expected values based on inputted constants, velocity, and estimated acceleration
    
    # lExpectedVolt <- file$Drive.left_vel*input$velConst + leftAccel*input$accelConst + input$voltConst
    # rExpectedVolt <- file$Drive.right_vel*input$velConst + rightAccel*input$accelConst + input$voltConst
    
    #Calculate residuals
    
    # lResid <- (lExpectedVolt - file$Drive.left_voltage)^2
    # rResid <- (rExpectedVolt - file$Drive.right_voltage)^2
    
    # get data based on input$dataVal from ui.R
    
    xData <- logTimes
    yData <- file[-1, input$dataVal]
    if (is.element(input$dataVal,c("left.error","right.error"))) {
      
    }
    else {
      yData <- file[-1, input$dataVal]
    }
    name <- input$dataVal
    curData <- data.frame(xData, yData)
    
    
    # draw plot with specified data values
    if (input$plotType == "scatter"){plot <- ggplot(data=curData, aes(x=xData, y=yData)) + geom_point() + labs(x = "Time", y = name)}
    else{(plot <- ggplot(data=curData, aes(x=xData, y=yData)) + geom_line() + labs(x = "Time", y = name))}
    if(input$smooth == TRUE){plot <- plot + geom_smooth(method="loess",span=input$span)}
    plot
  })
  
})
