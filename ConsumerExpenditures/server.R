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
    x <- dat[, c("Category", "Year", input$region)];
    y <- x[x$Category == input$topic,];
    y <- y[,2:3]
  })
       
  # Fill in the spot we created for a plot
#   output$textStr <- renderText({
#     cx <- datInput()
#   })
  output$meanPlot <- renderPlot({
     
    # Render a plot
    cx <- datInput();
    gg <- ggplot(cx, aes_string(colnames(cx)[1], colnames(cx)[2]));
    gg + geom_line(size = 2,
                  color = "darkgreen")
    
    
  })
})