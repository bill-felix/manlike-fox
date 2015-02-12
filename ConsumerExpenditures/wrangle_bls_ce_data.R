## # # # # # ###
##     wrangle##
## bls_ce_data##
## # # # # # ###

library(sqldf)
library(plyr)
library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)

options(max.print = 100) ## limit printed results

setwd("~/GitHub/manlike-fox")

ins_dat <- read.csv("lifeins_dat.csv")
ent_dat <- read.csv("entrtn_dat.csv")
guide <- read.delim("cx.characteristics.txt", header = F, sep = "\t")

demo_codes <- guide$V1
ch_codes <- guide$V2

str_detect(ins_dat$Series.ID, as.character(sample(demo_codes,1)))

ins_dat %<>%
  mutate(d_code = ifelse(ins_dat$Series.ID %in% demo_codes), guide$V3)
