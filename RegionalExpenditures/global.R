# shiny demonstration #

# setwd("~/GitHub/manlike-fox/shinyapp")
tall <- read.table("tall.csv", header =T, sep = ",")
yrs <- 1984:2013
topics <- unique(tall$category)

## lets get shiny
# library(shiny)
#
# setwd("~/GitHub/r/data/bls_ce/shinyapp")
# # function to run the shinyapp using the ui.R and server.R scripts in the current working directory
# runApp()
#
#
#
# devtools::install_github('rstudio/shinyapps')
# shinyapps::setAccountInfo(name='manlike-fox', token='23CA945821D34AB5F9C1A7F7D198E8B9', secret='PCalEiRzYARgGUp+tzMIK3NpGcR0G1rqb70/omOi')
#
# library(shinyapps)
# shinyapps::deployApp('~/GitHub/manlike-fox/regions')
