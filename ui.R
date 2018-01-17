#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
logNames <- names(read.csv("logs/telemetryLog-1969.12.31.20.51.15.csv", header=TRUE)[,-1])
sliderLabel <- "Data Type"
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Robot Fault Detection"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("fileName", label="Selected Log File", choice = list.files("logs/")),
      selectInput("dataVal", label = "Data Type", logNames),
      selectInput("plotType", label = "Plot Type", c("Scatterplot" = "scatter", "Line plot" = "line")),
      checkboxInput("smooth","Smooth"),
      conditionalPanel(
        condition = "input.smooth == true",
        sliderInput("span", "Span", min=0.01, max=1, value=0.1, step=0.01)
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
