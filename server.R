library(shiny)
library(devtools)
library(ggplot2)
library(tidyr)
library(projprep)
#devtools::load_all(pkg = file.path('..', 'projprep'))

source(file = 'constants.R')
source(file = 'data.R')
source(file = 'names.R')


shinyServer(function(input, output) {

  #reactive constants
  this_mlbid <- reactive(find_mlbid(input$in1, match_df))
  this_hit_pitch <- reactive(
    find_name_metadata(this_mlbid(), match_df, 'hit_pitch')
  )
  this_position <- reactive(
    find_name_metadata(this_mlbid(), match_df, 'pos')
  )
  
  #render reactive fields as text
  output$this_player <- renderText(input$in1)
  output$this_mlbid <- renderText(this_mlbid())
  output$this_hit_pitch <- renderText(this_hit_pitch())
  output$this_position <- renderText(this_position())
  output$reactive_string <- renderText(
    paste(
      input$in1, this_mlbid(), this_hit_pitch(), this_position(), sep = ' / '
    )
  )
  
  #TODO: return stats for this mlbid

  #PLOTS
  #stat dist
  output$big_plot <- renderPlot({
    stat_dist_all(
      pp_list = all_proj, 
      playerid = this_mlbid(), 
      player_pos = this_position(),
      hit_pitch = this_hit_pitch()
    )}
  )
  
  #price grid
  output$price_plot <- renderPlot({
    price_table(
      pp_list = all_proj, 
      playerid = this_mlbid(), 
      hit_pitch = this_hit_pitch()
    )},
  height = 80
  )
  

})

