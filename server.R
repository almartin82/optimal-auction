library(shiny)
library(devtools)
devtools::load_all(
  pkg = "/Users/almartin/Google\ Drive/repositories/projprep"
)

source(file = 'constants.R')
source(file = 'data.R')
source(file = 'names.R')


shinyServer(function(input, output) {

  #name of the player selected for auction
  output$this_player <- renderText(input$in1)
  
  #look up player selected and return id
  output$this_mlbid <- renderText(
    find_mlbid(input$in1, match_df)
  )
  
  #return stats for this mlbid
  
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- proj1$h_final[, 'value'] %>% unlist()
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  
  
})
