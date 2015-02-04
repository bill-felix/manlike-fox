# shiny demonstration # 

setwd("~/GitHub/manlike-fox/shinyapp")
life <- as.matrix(read.table("life.txt"))

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
# shinyapps::setAccountInfo(name='manlike-fox', token='CCF85FE350DD646C513FE1BE50D68114', secret='4djBSd5s3iPsYmIQWO/BmkORukNMJGHiTpBjCOc3')
# 
# library(shinyapps)
# shinyapps::deployApp('~/GitHub/r/data/bls_ce/shinyapp')
