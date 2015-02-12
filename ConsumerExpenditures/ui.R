# ui.R

library(shiny)

# lets look at life ins.
source('global.R')

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Select some parameters"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar
      sidebarPanel(
        selectInput('topic', 'Topic:', #select a singleInput
                    choices=levels(topics)),
        
        selectInput("region", "Region:", # select a single Input
                    choices= c("Midwest", "Northeast", "South", "West")),
      
        hr(), # horizontal rule line
        helpText("BLS - Consumer Expenditure Data. (2013)") # citation
      ),
    
    # Create a spot for the barplot
      mainPanel(
#         textOutput("textStr")
        plotOutput("meanPlot")
      )
    )
  )
)