#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
logNames <- names(read.csv("logs/telemetryLog-2018.01.21.10.46.10.csv", header=TRUE)[,-1])
# sliderLabel <- "Data Type"
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Robot Fault Detection"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("fileName", label="Selected Log File", choice = list.files("logs/")),
      selectInput("dataVal", label = "Data Type", logNames),
      # selectInput("dataVal", label = "Data Type", c(logNames,"Left Estimated Acceleration"="lAccel","Right Estimated Acceleration"="rAccel","Left Voltage Residuals"="lResid", "Right Voltage Residual"="rResid")),
      selectInput("plotType", label = "Plot Type", c("Scatterplot" = "scatter", "Line plot" = "line")),
      checkboxInput("smooth","Smooth"),
      conditionalPanel(
        condition = "input.smooth == true",
        sliderInput("span", "Span", min=0.01, max=1, value=0.1, step=0.01)
      )
      # conditionalPanel(
      #   condition = "input.dataVal == 'Drive.left_voltage' || input.dataVal == 'Drive.right_voltage' || input.dataVal == 'Voltage Residuals'",
      #   conditionalPanel(
      #     condition = "input.dataVal == 'Drive.left_voltage' || input.dataVal == 'Drive.right_voltage'",
      #     checkboxInput("display","Display Estimate")
      #   ),
      #   numericInput("velConst","Velocity Constant",value=1),
      #   numericInput("accelConst", "Acceleration Constant",value=1),
      #   numericInput("voltConst","Voltage Intercept",value=1)
      # )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
