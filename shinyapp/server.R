# server.R

library(shiny)
library(dplyr)
library(magrittr)
# setwd("~/GitHub/r/data/bls_ce/shinyapp")

# lets look at life ins.
source('global.R')

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Fill in the spot we created for a plot
  output$meanPlot <- renderPlot({
  
    # Render a barplot
    barplot(life[,input$region], 
            main=input$region,
            ylab="Avg Spending",
            xlab="Year")
  })
})
