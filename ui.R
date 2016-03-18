library(shiny)
library(tidyr)
library(ggplot2)
source(file = 'constants.R')
source(file = 'data.R')
source(file = 'names.R')

shinyUI(fluidPage(
  tags$style(
    type = 'text/css', 
    ".selectize-input { font-size: 12px; line-height: 12px;} .selectize-dropdown { font-size: 10px; line-height: 10px; } .well {padding: 5px;} .form-group { margin-bottom: 5px;}"
  ),
  
  fluidRow(
    column(3,
      wellPanel(
        selectInput(
          inputId = 'in1', 
          label = 'Player', 
          choices = c(Player = '', player_disp_name), 
          multiple = FALSE,
          selectize = TRUE
        )
      )
    ),
    column(3,
      verbatimTextOutput('this_player')
    )
  ),
  
  #row 2
  fluidRow(
    column(12, plotOutput('big_plot'))
  )
))


# column(3, plotOutput('rbi_plot')),
# column(3, plotOutput('sb_plot')),
# column(3, plotOutput('tb_plot')),
# column(4, plotOutput('obp_plot'))
