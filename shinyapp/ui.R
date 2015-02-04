# ui.R

library(shiny)
# setwd("~/GitHub/r/data/bls_ce/shinyapp")

# lets look at life ins.
source('global.R')

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Average Spending for Life Insurance"
    ),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar;
      sidebarPanel(
        selectInput("region", "Region:", # select a single Input
                    choices=dimnames(life)[[2]]),
        hr(),
        helpText("BLS - CE Data. (2013)") # citation
      ),
    
    # Create a spot for the barplot
      mainPanel(
        plotOutput("meanPlot")
      )
    )
  )
)