library(shiny)
library(devtools)
library(ggplot2)
library(tidyr)
devtools::load_all(pkg = file.path('..', 'projprep'))

source(file = 'constants.R')
source(file = 'data.R')
source(file = 'names.R')


shinyServer(function(input, output) {

  #reactive constants
  this_mlbid <- reactive(find_mlbid(input$in1, match_df))
  this_hit_pitch <- reactive(
    find_name_metadata(this_mlbid(), match_df, 'hit_pitch')
  )
  
  #render reactive fields as text
  output$this_player <- renderText(input$in1)
  output$this_mlbid <- renderText(this_mlbid())
  output$this_hit_pitch <- renderText(this_hit_pitch())
  
  #TODO: return stats for this mlbid

  #PLOTS
  #stat dist
  output$big_plot <- renderPlot({
    stat_dist_all(
      pp_list = all_proj, 
      playerid = this_mlbid(), 
      player_pos = 'OF',
      hit_pitch = 'h'
    )
  })
  
  #price grid
  

})

