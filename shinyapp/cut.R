# shiny demonstration # 

library(stringr)
library(plyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)


setwd("~/GitHub/r/data/bls_ce/shinyapp")
options(max.print = 100)

values <- read.table("cx.AllData.txt", header =T, sep ="\t")
series <- read.table("cx.AllSeries.txt", header =T, sep ="\t")

dat <- merge(values, series, by = "series_id", all.x =T)

wide_data <- dat %>%
  group_by(series_title, year) %>%
  summarise(mean = round(mean(value, na.rm =T, trim = .025), digits = 2)) %>%
  separate(series_title, into = c("category", "demo_code", "char_text"), sep ="-", remove =T, extra ="merge") 