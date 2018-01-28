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
  
  #Reads file, and also deletes any uncompleted lines, and where the `time` column seems to stop iterating.
  
  usefile <- reactive({
    usefile <- read.csv(paste("logs/",input$fileName, sep=""), header=TRUE)
    usefile <- head(usefile[c(1,which(rowSums(diff(as.matrix(usefile[,grep('time',names(usefile))])))!=0)+1),],-1)
    usefile
  })
  
  #Estimate acceleration based on velocity
  
  right.accel <- reactive({
    smoothDerivative(usefile()$time, usefile()$right.velocity)
  })
  left.accel <- reactive({
    smoothDerivative(usefile()$time, usefile()$left.velocity)
  })
  
  #Filters data based on acceleration and threshold (set by user).
  
  left.adj <- reactive({
    if (input$accelThreshold > 0){
      subset(usefile(), abs(left.accel()) < input$accelThreshold)
    }
    else{
      usefile()
    }
  })
  right.adj <- reactive({
    if (input$accelThreshold > 0){
      subset(usefile(), abs(right.accel()) < input$accelThreshold)
    }
    else{
      usefile()
    }
  })
  
  #Calculates standard deviation of error of data (after acceleration filtering).
  
  left.sd <- reactive({
    if (input$filterType == "filtL"){sd(left.adj()$left.error)}
    else{sd(right.adj()$left.error)}
  })
  right.sd <- reactive({
    if (input$filterType == "filtL"){mean(left.adj()$right.error)}
    else{mean(right.adj()$right.error)}
  })
  
  #Calculates mean of error of data (after acceleration filtering).
  
  left.mean <- reactive({
    if (input$filterType == "filtL"){mean(left.adj()$left.error)}
    else{mean(right.adj()$left.error)}
  })
  
  right.mean <- reactive({
    if (input$filterType == "filtL"){mean(left.adj()$right.error)}
    else{mean(right.adj()$right.error)}
  })
  
  tableData <- reactive({
    setNames(data.frame(left.mean(), right.mean(), left.sd(), right.sd()), c("Left Error Mean", "Right Error Mean", "Left Error Standard Deviation", "Right Error Standard Deviation"))
  })
  
  
  #####MISC (Unused, unadapted to new format. May be used in future.)
    
    #Estimate expected values based on inputted constants, velocity, and estimated acceleration
    
    # lExpectedVolt <- usefile$Drive.left_vel*input$velConst + left.accel*input$accelConst + input$voltConst
    # rExpectedVolt <- usefile$Drive.right_vel*input$velConst + right.accel*input$accelConst + input$voltConst
    
    #Calculate residuals
    
    # lResid <- (lExpectedVolt - usefile$Drive.left_voltage)^2
    # rResid <- (rExpectedVolt - usefile$Drive.right_voltage)^2
  
  #####END
    
  output$distPlot <- renderPlot({
    # get data based on input$dataVal from ui.R
    if (input$accelFilter){
      if (input$filterType == "filtL") {
        xData <- left.adj()$time
        yData <- left.adj()[[input$dataVal]]
      }
      else {
        xData <- right.adj()$time
        yData <- right.adj()[[input$dataVal]]
      }
    }
    else{
      xData <- usefile()[-1,1]
      yData <- usefile()[-1, input$dataVal]
    }
    # if (is.element(input$dataVal,c("left.error","right.error"))) {
    #   
    # }
    # else {
    #   yData <- usefile[-1, input$dataVal]
    # }
    name <- input$dataVal
    curData <- data.frame(xData, yData)
    
    
    # draw plot with specified data values
    if (input$plotType == "scatter"){plot <- ggplot(data=curData, aes(x=xData, y=yData)) + geom_point() + labs(x = "Time", y = name)}
    else{(plot <- ggplot(data=curData, aes(x=xData, y=yData)) + geom_line() + labs(x = "Time", y = name))}
    if(input$smooth == TRUE){plot <- plot + geom_smooth(method="loess",span=input$span)}
    plot
  })
  output$stats <- renderTable({
    tableData()
  })
    
  
})
