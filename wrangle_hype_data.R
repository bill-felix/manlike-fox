## # # # # ###
##   wrangle##
## hype_data##
## # # # # ###

library(plyr)
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)

options(max.print = 100) ## limit printed results

# import data (hype for now)
dat <- data.frame(id = sample(8000:9999, 100),
                  u_long = rnorm(100, mean = 84.5352946, sd = .00005),
                  u_lat = rnorm(100, mean = 39.1651805, sd = .00005),
                  twc = runif(100, min = 45, max = 155),
                  cinbell = runif(100, min = 55, max = 95),
                  comcast = runif(100, min = 75, max = 80))

colmeans <- colMeans(dat[,2:4])
colmeans

# ggplot2
