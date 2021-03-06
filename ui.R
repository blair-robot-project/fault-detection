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

logNames <- names(read.csv(paste("logs/",list.files("logs/",pattern="^telemetryLog")[1],sep=""),header=TRUE))

##### JAVASCRIPT #####
script <- "$('tbody:not(:first) tr td:nth-child(3)').each(function() {
var val = $(this).text();

// Catch exceptions outside of range
if (val > 1) {var val = 1;}

// Find value's position relative to range
var pos = 510 * val;

// Generate RGB code
if (pos < 255){
red = 255;
green = parseInt((0 + pos).toFixed(0));
}
else {
red = parseInt((510 - pos).toFixed(0));
green = 255;
}
clr = 'rgb('+red+','+green+',0)';

// Apply to cell
if (val > 0.2){
  clearInterval(blinker)
  $(this).css('background-color', clr);
}
else{
  if (typeof blinker !== 'undefined'){clearInterval(blinker);}
  setInterval(function(){
    $('tbody:not(:first) tr td:nth-child(3)').css('background-color', function(){
    this.s = !this.s
    return this.s ? 'rgb(255,0,0)':'rgb(255,255,255)'
    })
  }, 1000)
}
})"


##### REQUIRED LOG NAME FORMATTING #####
# telemetryLog-YYYY.mm.dd.hh.mm.ss.csv

# Formats filenames into easily readable date and time format.

getDateTime <- function(){
  dateTime <- gsub(".csv","",gsub("telemetryLog-", "", list.files("logs/",pattern="^telemetryLog"), fixed=TRUE),fixed=TRUE)
  time <- substring(dateTime, lapply(gregexpr(pattern='.',dateTime,fixed=TRUE), function(x) x[3] + 1))
  date <- substring(dateTime, 0, lapply(gregexpr(pattern='.',dateTime,fixed=TRUE), function(x) x[3] - 1))
  paste(format(as.Date(date,format="%Y.%m.%d"),format="%B %d %Y"), gsub(".",":",time,fixed=TRUE))
}


# Defines the UI.
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Log Viewer"),
  
  # Sidebar with inputs to determine desired data to display
  sidebarLayout(
    sidebarPanel(
      
      # Selects a file from a preselected folder to read.
      selectInput("fileName", label="Selected Log File", choice = getDateTime()),# selected = "February 04 2018 10:47:26"),
      
      # Allows user to choose a data set to display (as y-value) on plot.
      selectInput("dataValX", label = "X Data Type", choice = logNames, selected="time"),
      selectInput("dataValY", label = "Y Data Type", choice = logNames, selected="PDP.temperature"),
      
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
      
      # Filters out points if checked, using acceleration and given acceleration threshold.
      # checkboxInput("filter","Filter?",value=FALSE),
      
      # Allows user to input an acceleration threshold.
      #conditionalPanel(
      #  condition = "input.filter",
        # selectInput("filterType", "Filter by...", c("left.accel","right.accel",logNames)),
        # radioButtons("filterDirection","Direction",c("<"="less","≤"="lesseq","="="eq","≥"="greatereq",">"="greater"), inline=TRUE),
        # numericInput("threshold","Threshold", value=0)
      #)
      ##### OUTDATED #####
      
      fluidRow(
        column(6, actionButton('addFilter', "Add Filter")),
        offset=6
      ),
      tags$hr(),
      tags$div(id='filters'),
      width = 4
    ),
    
    # Shows two tabs, one with the plotted data and the other with statistics for given data.
    mainPanel(
       tabsetPanel(
         tabPanel("Plot", plotOutput("distPlot")),
         tabPanel("Stat Tables", tableOutput("error"), tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });'))), tableOutput("resist.bP"), tableOutput("dataResist1"),tableOutput("dataResist2"),tableOutput("dataResist3"),tableOutput("dataResist4"),tableOutput("dataResist5"),tableOutput("dataResist6"),tableOutput("dataResist7"),tableOutput("dataResist8"),tableOutput("dataResist9"),tableOutput("dataResist10"),tableOutput("dataResist11"),tableOutput("dataResist12"),tableOutput("dataResist13"),tableOutput("dataResist14"))
       )
    )
  )
))
