#
# Server logic of Robot Fault Detection.
# Uses logs and user input to graph relevant data, and
# processes data to provide statistics on expected values
# and to find outliers (unexpected values).
#
# Employs R Shiny.
#
# Bryan Li
#

library(shiny)
library(ggplot2)

# Function that estimates the derivative of a dataset via splines.

smoothDerivative <- function(x, y){
  #Generates spline based on inputted data.
  spl <- smooth.spline(x, y)
  
  #Predicts derivative of spline.
  pred.prime <- predict(spl, deriv=1)
  
  #Returns prediction.
  return(pred.prime$y)
}

# Define server logic required to process data and display page.
shinyServer(function(input, output) {
  
  # Reads file, and also deletes any uncompleted lines along with where the 'time' column seems to stop iterating.
  usefile <- reactive({
    usefile <- read.csv(paste("logs/",input$fileName, sep=""), header=TRUE)
    usefile <- head(usefile[c(1,which(rowSums(diff(as.matrix(usefile[,grep('time',names(usefile))])))!=0)+1),],-1)
    usefile
  })
  
  # Gets names of data columns in file.
  logNames <- reactive({
    names(usefile())
  })
  
  # Estimate left and right acceleration based on velocity.
  right.accel <- reactive({
    smoothDerivative(usefile()$time, usefile()$right.velocity)
  })
  left.accel <- reactive({
    smoothDerivative(usefile()$time, usefile()$left.velocity)
  })
  
  # Filters data based on acceleration and acceleration threshold (set by user).
  left.adj <- reactive({
    if (input$accelThreshold > 0){ subset(usefile(), abs(left.accel()) < input$accelThreshold) }
    else{ usefile() }
  })
  right.adj <- reactive({
    if (input$accelThreshold > 0){ subset(usefile(), abs(right.accel()) < input$accelThreshold) }
    else{ usefile() }
  })
  
  # Calculates standard deviation of error of data (after acceleration filtering).
  left.sd <- reactive({
    if (input$filterType == "filtL"){ sd(left.adj()$left.error) }
    else{ sd(right.adj()$left.error) }
  })
  right.sd <- reactive({
    if (input$filterType == "filtL"){ mean(left.adj()$right.error) }
    else{ mean(right.adj()$right.error) }
  })
  
  # Calculates mean of error of data (after acceleration filtering).
  left.mean <- reactive({
    if (input$filterType == "filtL"){ mean(left.adj()$left.error) }
    else{ mean(right.adj()$left.error) }
  })
  right.mean <- reactive({
    if (input$filterType == "filtL"){ mean(left.adj()$right.error) }
    else{ mean(right.adj()$right.error) }
  })
  
  # Generates data frame with desired values, to use in table.
  tableData <- reactive({
    setNames(data.frame(left.mean(), right.mean(), left.sd(), right.sd()), c("Left Error Mean", "Right Error Mean", "Left Error Standard Deviation", "Right Error Standard Deviation"))
  })
  
  
  ##### OUTDATED #####
  # Unused, unadapted to new format. May be used in future.
    
    #Estimate expected values based on inputted constants, velocity, and estimated acceleration
    
    # lExpectedVolt <- usefile$Drive.left_vel*input$velConst + left.accel*input$accelConst + input$voltConst
    # rExpectedVolt <- usefile$Drive.right_vel*input$velConst + right.accel*input$accelConst + input$voltConst
    
    #Calculate residuals
    
    # lResid <- (lExpectedVolt - usefile$Drive.left_voltage)^2
    # rResid <- (rExpectedVolt - usefile$Drive.right_voltage)^2
  
  ##### OUTDATED #####
    
  output$distPlot <- renderPlot({
    # Uses data based on input$dataVal.
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
    
    #Generates a data frame for data to plot on graph.
    curData <- data.frame(xData, yData)
    
    # Draws plot with specified data values. Type depends on user input.
    if (input$plotType == "scatter"){ plot <- ggplot(data=curData, aes(x=xData, y=yData)) + geom_point() + labs(x = "Time", y = input$dataVal) }
    else{ (plot <- ggplot(data=curData, aes(x=xData, y=yData)) + geom_line() + labs(x = "Time", y = input$dataVal)) }
    
    # Generates a smoothed line to graph over plot based on user input.
    if(input$smooth == TRUE){ plot <- plot + geom_smooth(method="loess",span=input$span) }
    
    # Final result, to store in distPlot.
    plot
  })
  
  # Generates table data to display.
  output$stats <- renderTable({
    tableData()
  })
  
  # Allows for input that adapts to column name changes in file.
  # output$chooseData <- renderUI({
  #   selectInput("dataVal", label = "Data Type", choice = logNames()[])
  # })
    
  
})
