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

##### CUSTOM FUNCTIONS #####

# Function that estimates the derivative of a dataset via splines.
smoothDerivative <- function(x, y){
  #Generates spline based on inputted data.
  spl <- smooth.spline(x, y)
  
  #Predicts derivative of spline.
  pred.prime <- predict(spl, deriv=1)
  
  #Returns prediction.
  return(pred.prime$y)
}

##### REQUIRED LOG NAME FORMATTING #####
# telemetryLog-YYYY.mm.dd.hh.mm.ss.csv

# Parses date and time received from UI and converts back to file name.

parseDateTime <- function(string){
  time <- substring(string, lapply(gregexpr(pattern=' ',string,fixed=TRUE), function(x) x[3] + 1))
  date <- substring(string, 0, lapply(gregexpr(pattern=' ',string,fixed=TRUE), function(x) x[3] - 1))
  paste("logs/telemetryLog-",paste(format(as.Date(date,format="%B %d %Y"),format="%Y.%m.%d"), gsub(":",".",time,fixed=TRUE), sep="."),".csv",sep="")
}

# Define server logic required to process data and display page.
shinyServer(function(input, output, session) {
  
  ##### VARIABLES #####
  
  # Reads file, and also deletes any uncompleted lines along with where the 'time' column seems to stop iterating.
  usefile <- reactive({
    usefile <- read.csv(parseDateTime(input$fileName), header=TRUE)
    usefile <- head(usefile[c(1,which(rowSums(diff(as.matrix(usefile[,grep('time',names(usefile))])))!=0)+1),],-1)
    usefile$time <- usefile$time / 1000
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
  
  filter <- character(0)
  
  makeReactiveBinding("aggregFilterObserver")
  aggregFilterObserver <- list()
  
  observeEvent(input$addFilter, {
    add <- input$addFilter
    filterId <- paste0('filter', add)
    colFilter <- paste0('colFilter', add)
    lwrBoundNum <- paste0('lowerBound', add)
    uprBoundNum <- paste0('upperBound', add)
    removeFilter <- paste0('removeFilter', add)
    exclusivity <- paste0('exclusivity', add)
    insertUI(
      selector = '#filters',
      ui = tags$div(id = filterId,
                    actionButton(removeFilter, label = "Remove filter", style = "float: right;"),
                    selectInput(colFilter, label = paste0("Filter", add), choices = logNames()),
                    numericInput(lwrBoundNum, label = "Lower Bound", value=0, width = 4000),
                    numericInput(uprBoundNum, label = "Upper Bound", value=10, width = 4000),
                    checkboxInput(exclusivity, label = "Within Boundaries?", value=TRUE)
      )
    )
    
    observeEvent(input[[colFilter]], {
      
      filteredCol <- usefile()[[input[[colFilter]]]]
      
      updateNumericInput(session, lwrBoundNum, min=min(filteredCol), max=max(filteredCol))
      updateNumericInput(session, uprBoundNum, min=min(filteredCol), max=max(filteredCol))
      aggregFilterObserver[[filterId]]$col <<- input[[colFilter]]
      aggregFilterObserver[[filterId]]$rows <<- NULL
    })
    
    observeEvent(c(input[[lwrBoundNum]], input[[uprBoundNum]], input[[colFilter]], input[[exclusivity]]), {
      if (input[[exclusivity]]){
        rows <- usefile()[[input[[colFilter]]]] >= input[[lwrBoundNum]]
        rows <- "&"(rows, usefile()[[input[[colFilter]]]] <= input[[uprBoundNum]])
      }
      else{
        rows <- usefile()[[input[[colFilter]]]] < input[[lwrBoundNum]]
        rows <- "|"(rows, usefile()[[input[[colFilter]]]] > input[[uprBoundNum]])
      }
      
      aggregFilterObserver[[filterId]]$rows <<- rows
      
    })
    
    observeEvent(input[[removeFilter]], {
      removeUI(selector = paste0('#', filterId))
      
      aggregFilterObserver[[filterId]] <<- NULL
      
    })
  })
  ##### OUTDATED #####
  # Filters data based on acceleration and acceleration threshold (set by user).
  # toAdjust <- reactive({
  #     if (input$filterType == "left.accel"){abs(left.accel())}
  #     else if (input$filterType == "right.accel"){abs(right.accel())}
  #     else {abs(usefile()[[input$filterType]])}
  # })
  # adjusted <- reactive({
  #   if (input$filter == TRUE){
  #     if (input$filterDirection == "greater") {subset(usefile(), toAdjust() > input$threshold)}
  #     else if (input$filterDirection == "less") {subset(usefile(), toAdjust() < input$threshold)}
  #     else if (input$filterDirection == "greatereq") {subset(usefile(), toAdjust() >= input$threshold)}
  #     else if (input$filterDirection == "lesseq") {subset(usefile(), toAdjust() <= input$threshold)}
  #     else {subset(usefile(), toAdjust() == input$threshold)}
  #   }
  #   else { usefile() }
  # })
  
  adjusted <- reactive({
    toAdjust <- rep(TRUE,nrow(usefile()))
    #dataSet <- usefile()
    lapply(aggregFilterObserver, function(filter){
      #dataSet <<- dataSet[which(dataSet[[filter$col]] %in% filter$rows), ]
      toAdjust <- "&"(toAdjust, filter$rows)
    })
    subset(usefile(), toAdjust)
  })
  
  # Calculates standard deviation of error of data (after acceleration filtering).
  left.sd <- reactive({
    sd(adjusted()$left.error)
  })
  right.sd <- reactive({
    sd(adjusted()$right.error)
  })
  
  # Calculates mean of error of data (after acceleration filtering).
  left.mean <- reactive({
    mean(adjusted()$left.error)
  })
  right.mean <- reactive({
    mean(adjusted()$right.error)
  })
  
  # Linear regression
  
  batteryPDP.summary <- reactive({
    summary(lm(formula=PDP.voltage~PDP.current, usefile()[,c("PDP.voltage","PDP.current")]))
  })
  
  ##### TABLE GENERATION #####
  
  errorData <- reactive({
    data.frame(
      left.mean(),
      right.mean(),
      left.sd(),
      right.sd()
    )
  })
  
  errorLabels <- c(
    "Left Error Mean",
    "Right Error Mean",
    "Left Error Standard Deviation",
    "Right Error Standard Deviation"
  )
  
  resistData <- reactive({
    data.frame(
      -batteryPDP.summary()$coefficients["PDP.current","Estimate"],
      batteryPDP.summary()$coefficients["(Intercept)","Estimate"],
      batteryPDP.summary()$coefficients["PDP.current","Pr(>|t|)"]
    )
  })
  
  resistLabels <- c(
    "Battery-PDP Resistance",
    "Battery-PDP Voltage Intercept",
    "Battery-PDP P-Value"
  )
  
  
  ##### OUTDATED CODE #####
  # Unused, unadapted to new format. May be used in future.
    
    #Estimate expected values based on inputted constants, velocity, and estimated acceleration
    
    # lExpectedVolt <- usefile$Drive.left_vel*input$velConst + left.accel*input$accelConst + input$voltConst
    # rExpectedVolt <- usefile$Drive.right_vel*input$velConst + right.accel*input$accelConst + input$voltConst
    
    #Calculate residuals
    
    # lResid <- (lExpectedVolt - usefile$Drive.left_voltage)^2
    # rResid <- (rExpectedVolt - usefile$Drive.right_voltage)^2
  
  ##### DISPLAY #####
    
  output$distPlot <- renderPlot({
    # Uses data based on input$dataVal.
    xData <- adjusted()[[input$dataValX]]
    yData <- adjusted()[[input$dataValY]]

    #Generates a data frame for data to plot on graph.
    curData <- data.frame(xData, yData)

    # Draws plot with specified data values. Type depends on user input.
    if (input$plotType == "scatter"){ plot <- ggplot(data=curData, aes(x=xData, y=yData)) + geom_point() + labs(x = input$dataValX, y = input$dataValY) }
    else{ (plot <- ggplot(data=curData, aes(x=xData, y=yData)) + geom_line() + labs(x = input$dataValX, y = input$dataValY)) }

    # Generates a smoothed line to graph over plot based on user input.
    if(input$smooth == TRUE){ plot <- plot + geom_smooth(method="loess",span=input$span) }

    # Final result, to store in distPlot.
    plot
  })
  
  # Generates table data to display.
  output$error <- renderTable({
    setNames(errorData(), errorLabels)
  })
  
  output$resist <- renderTable({
    setNames(resistData(), resistLabels)
  })
  
  # Allows for input that adapts to column name changes in file.
  # output$chooseData <- renderUI({
  #   selectInput("dataVal", label = "Data Type", choice = logNames()[])
  # })
    
  
})
