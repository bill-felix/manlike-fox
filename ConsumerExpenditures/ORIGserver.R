# server.R

library(shiny)
library(dplyr)
library(magrittr)
# setwd("~/GitHub/r/data/bls_ce/shinyapp")

# lets look at life ins.
source('global.R')

# Define a server for the Shiny app
shinyServer(function(input, output) {
  # Return the cols
  datInput <- reactive({
    x <- dat[, c("Category", "Year", input$region)];
    y <- x[x$Category == input$topic,];
    y
  })
       
  # Fill in the spot we created for a plot
  output$meanPlot <- renderPlot({
     
  
    # Render a barplot
    cx <- datInput()
    plot(cx$Year, cx[,3],
         type = "b",
         xlab = "Years",
         ylab = "Avg. Spend")
  })
})
