library(shiny)

source(file = 'constants.R')
source(file = 'data.R')
source(file = 'names.R')

shinyUI(fluidPage(

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      #player picker
      selectInput(
        inputId = 'in1', 
        label = 'Player', 
        choices = c(Player = '', player_disp_name), 
        multiple = FALSE,
        selectize = TRUE
      ),
      sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      )
    #end sidebarPanel
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput('this_player'),
      verbatimTextOutput('this_mlbid'),
      plotOutput("distPlot")
    )
  #end sidebarLayout
  )
))