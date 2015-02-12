# shiny demonstration # 

library(stringr)
library(plyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)


# setwd("~/GitHub/r/data/bls_ce")
options(max.print = 100)

values <- read.table("cx.data.1.AllData.txt", header =T, sep ="\t")

series <- read.table("cx.series.txt", header =T, sep ="\t")

demo <- read.table("cx.demographics.txt", header =T, sep ="\t", stringsAsFactors =F, row.names =NULL)
demo <- demo[,1:2]
colnames(demo) <- c("demo_code", "demo_text")

char <- read.table("cx.characteristics.txt", header =T, sep ="\t", row.names =NULL)
char <- char[,1:3]
colnames(char) <- c("demo_code", "char_code", "char_text")

dat <- merge(values, series, by = "series_id", all.x =T)
wide_data <- dat %>%
  group_by(series_title, year) %>%
  summarise(mean = mean(value)) %>%
  separate(series_title, into = c("category", "demo_code", "char_text"), sep ="-", remove =T, extra ="merge") 

region_wide <- wide_data %>%
  filter(demo_code == "LB11") %>%
  spread(char_text, mean) %>%
  select(c(1:4,6,8,10,12)) %>%
  arrange(category, year)

colnames(region_wide) <- c("Category", "Demo_Code", "Year", "All", "Midwest", "Northeast", "South", "West")

# setwd("~/GitHub/manlike-fox/shinyapp")
write.csv(region_wide, "cx.csv", row.names =F)

life_dat <- region_wide %>%
  filter(grepl("Life", Category)) %>%
  select(c(3,5:8))

life <- as.matrix(life_dat[,2:5])
dimnames(life)[[1]] <- life_dat$Year

write.table(life, "life.txt")

setwd("shinyapps")



# life <- dat %>%
#   filter(subcategory_code == "INSPENSN", item_code == "LIFEINSR", demographics_code == "LB11") %>%
#   group_by(series_title,  year) %>%
#   summarise(count = n(),
#             mean = mean(value)) %>%
#   arrange(year)
# 
# life_w <- life %>%
#   select(series_title, mean, year) %>%
#   separate(series_title, into = c("Category", "cat_code", "Region"), sep ="-") %>%
#   spread(Region, mean) %>%
#   arrange(year)
#   
# 
# ggall <- ggplot(region_wide, aes(x = Year, y = All, color = Category))
# ggall + geom_line()
# 
# ## lets get shiny
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

# library(shinyapps)
# shinyapps::deployApp('~/GitHub/r/data/bls_ce/shinyapp')

tall_data <- dat %>%
  filter(demographics_code == "LB11") %>%
  group_by(series_title, year) %>%
  summarise(mean = mean(value))

tall_data <- separate(tall_data, series_title, into = c("category", "demo_code", "char_text"), sep ="-", remove =T, extra ="merge")
tall_data <- filter(tall_data, char_text == "Region of residence: midwest" | char_text == "Region of residence: northeast" |
                      char_text == "Region of residence: south" | char_text == "Region of residence: west")

setwd("~/GitHub/manlike-fox/shinyapp")
write.csv(tall_data, "tall.csv", row.names =F)
