library(shiny)
library(devtools)
library(ggplot2)
library(tidyr)
devtools::load_all(pkg = file.path('..', 'projprep'))

source(file = 'constants.R')
source(file = 'data.R')
source(file = 'names.R')


shinyServer(function(input, output) {

  #name of the player selected for auction
  output$this_player <- renderText(input$in1)
  
  #playerid
  this_mlbid <- reactive(find_mlbid(input$in1, match_df))
  
  #look up player selected and return id
  output$this_mlbid <- renderText(this_mlbid())
  
  #return stats for this mlbid

  #make plots
  output$big_plot <- renderPlot({
    stat_dist_all(
      pp_list = all_proj, 
      playerid = this_mlbid(), 
      player_pos = 'OF',
      hit_pitch = 'h'
    )
  })
  
})

