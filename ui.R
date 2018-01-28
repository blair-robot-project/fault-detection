#
# User-interface definition of Robot Fault Detection.
# Uses logs and user input to graph relevant data, and
# processes data to provide statistics on expected values
# and to find outliers (unexpected values).
#
# Employs R Shiny.
#
# Bryan Li
#

library(shiny)

logNames <- names(read.csv("logs/telemetryLog-2017.06.04.19.36.35.csv",header=TRUE))[c(-1,-2)]

# Defines the UI.
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Robot Fault Detection"),
  
  # Sidebar with inputs to determine desired data to display
  sidebarLayout(
    sidebarPanel(
      
      # Selects a file from a preselected folder to read.
      selectInput("fileName", label="Selected Log File", choice = list.files("logs/")),
      
      # Allows user to choose a data set to display (as y-value) on plot.
      selectInput("dataVal", label = "Data Type", choice = logNames),
      
      # Uses sliderInput from server.R in order to keep choice list updated.
      # uiOutput("chooseData"),
      
      # Serves same function as uncommented line directly above, except reactive to file.
      # Currently unused, as files should have the same column headers.
      
      # Allows user to choose a plot type.
      selectInput("plotType", label = "Plot Type", c("Scatterplot" = "scatter", "Line plot" = "line")),
      
      # Displays a smoothed line over the function if checked.
      checkboxInput("smooth","Smooth",value=FALSE),
      
      # Allows user to change span of smoothing function if smoothed line is enabled.
      conditionalPanel(
        condition = "input.smooth",
        sliderInput("span", "Span", min=0.01, max=1, value=0.1, step=0.01)
      ),
      
      ##### OUTDATED #####
      # Unused, unadapted to new format. May be used in future.
      
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
      
      ##### OUTDATED #####
      
      # Filters out points if checked, using acceleration and given acceleration threshold.
      checkboxInput("accelFilter","Filter by Acceleration",value=FALSE),
      
      # Allows user to input an acceleration threshold.
      conditionalPanel(
        condition = "input.accelFilter",
        radioButtons("filterType", "Filter by:", c("Left Accel."="filtL", "Right Accel."="filtR")),
        numericInput("accelThreshold","Acceleration Threshold",min = 0, max = 0.5, value = 0, step = 0.005),
        p("Use 0 for no filter.")
      )
    ),
    
    # Shows two tabs, one with the plotted data and the other with statistics for given data.
    mainPanel(
       tabsetPanel(
         tabPanel("Plot", plotOutput("distPlot")),
         tabPanel("Stats",tableOutput("stats"))
       )
    )
  )
))
