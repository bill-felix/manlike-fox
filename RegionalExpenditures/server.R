# panel test server.R

# alt server.R

library(shiny)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggvis)

# lets look at life ins.
source('global.R')

# Define a server for the Shiny app
shinyServer(function(input, output) {
  # Returns the dat
  datInput <- reactive({
    x <- tall[, c("category", "year", "char_text", "mean")];
    y <- x[x$category == input$topic,]
  })
       
  # Fill in the spot we created for a plot
#   output$textStr <- renderText({
#     cx <- datInput()
#   })
  output$meanPlot <- renderPlot({
     
    # Render a plot
    cx <- datInput();
    gg <- ggplot(cx, aes(year, mean));
    gg + geom_line(size = 2, color = "darkgreen") + facet_grid(~char_text)
    
    
  })
})