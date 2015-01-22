## # # # # ###
##   wrangle##
## hype_data##
## # # # # ###   

library(plyr)
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(ggplot2)
library(zoo)      

options(max.print = 250) ## limit printed results

# import data (hype for now)

rdate <- function(n, start, end) { ## start/end format = "yyyy/mm/dd"
    st <- as.POSIXct(as.Date(start))
    et <- as.POSIXct(as.Date(end))
    dt <- as.numeric(difftime(et,st,unit="sec")) ## calculates difftime
    ev <- runif(n, min = 1.1, max = dt) 
    rt <- st + ev ## adds difftime to start
}

submit <- function(h) {
  h <- arrange(h, timestamp) ## order observations by date
  h <- ddply(h, .(provider, zip, serv), function(x) {
    x$lowest <- cummin(x$price); 
    return(x)}) ## calculate lowest price to date
  h <- ddply(h, .(provider, zip, serv), function(x) {
    x$average <- round(cumsum(x$price)/seq_along(x$price), 2); 
    return(x)}) ## calculate avg price to date
  ## h <- ddply(h, .(provider,zip), function(x) {x$avg_price <- rollmean(x$price, 10, na.pad=TRUE, align = "right"); return(x)}) ## rolling avg by last 10 obvs
  h <- ddply(h, .(provider, zip, serv), function(x) {
    x$highest <- cummax(x$price); 
    return(x)}) ## calculate highest price to date 
  h %<>%
    mutate(thmbdown = ifelse(h$average < h$price, 1, 0))
}

populate_hype_data <- function(x,n) {
  x <- data.frame(sess_id = sample(50000000:99999999, n),
#                   long = rnorm(n, mean = 84.5352946, sd = .005),
#                   lat = rnorm(n, mean = 39.1651805, sd = .005),
                  zip = sample(zips, n, replace =T),
                  timestamp = rdate(n, "2014/01/01", "2015/12/31"),
                  serv = sample(services, n, replace =T, prob = c(.4, .4, .3)),
                  provider = sample(providers, n, replace =T, prob = c(.55, .35, .1)),
                  price = rnorm(n, mean = 75, sd = 10))
}

set.seed(6)
dat <- NULL
providers <- c("Time Warner Cable", "Cincinnati Bell", "Comcast")
services <- c("Cable/Internet","Cable", "Internet")
zips <- c("45223", "45202", "45206", "45201", "45239", 
          "45205", "45207", "45225", "45217", "45213", 
          "45214", "45216")
general_price <- round(runif(1000, min = 40, max = 160), 2)

dat <- populate_hype_data(dat,50000)

# artificially discount or raise providers
set.seed(80)
artificial <- rnorm(500, mean = 1, sd = .035)
counter <- 40
repeat {
  dat %<>%
  mutate(price = ifelse(zip == sample(zips, 1) | 
                        provider == sample(providers, 1) |
                        serv == sample(services, 1), 
                        price * sample(artificial, 1), price * 1))
  counter <- counter - 1
  if(counter < 0)
  break;   
}

# submit function simulates the submit button and returns results
dat <- submit(dat)

# lubridate functions for extraction
dat$year <- year(dat$timestamp)
dat$quarter <- quarter(dat$timestamp)
dat$month <- month(dat$timestamp)
dat$day <- day(dat$timestamp)

###### comparisons ######
a_dat <- dat %>%
  group_by(zip) %>%
  summarise(count = n(),
            avg = mean(price),
            high = max(price),
            low = min(price),
            sd = sd(price),
            diff_h_to_l = max(price) - min(price))

b_dat <- dat %>%
  group_by(provider) %>%
  summarise(count = n(),
            high = max(price),
            low = min(price),
            sd = sd(price),
            diff_h_to_l = max(price) - min(price)) 

d_dat <- dat %>%
  group_by(year, month, day) %>%
  summarise (count = n(),
             day_high = max(price),
             day_avg = mean(price),
             day_low = min(price),
             day_sd = sd(price))

d_dat <- zoo(d_dat)
  

############################## ggplot2 ###############################################

# Price and Average over time
ggplot(dat, aes(x=timestamp, y=average, color = zip)) + geom_point()

# daily
autoplot(d_dat[,5:8])

# Lines
g <- ggplot(dat, aes(x = day, y = average, color = provider))
g + geom_line()
g + geom_jitter(alpha = .3,position = position_jitter(width = .2))

g + geom_violin(alpha=0.5, color="gray") + 
  geom_jitter(alpha = .7, aes(color = "red"), position = position_jitter(width = .2)) + 
  coord_flip()

# Same plot as above, declaring only the data frame in ggplot().
# Note how the x and y aesthetics must now be declared in
# each geom_point() layer.
ggplot(dat) +
  geom_jitter(data = dat, aes(x = provider, y = price, color = zip), 
              alpha = .1, position = position_jitter(width = .2))

ggplot(dat) +
   geom_violin(aes(x = zip, y = price), aplha=0.5, color ="gray")
   
ggplot(dat, aes(y = price, x = zip, fill = provider)) + geom_boxplot()
base <- qplot(scale(dat[dat$zip == "45223", "price"]), geom = "density")
base + stat_function(fun = dnorm, colour = "red", 
                     arg = list(mean = 0)) # Northside's Distribution Curve vs. Normal  this is new

###########################################

write.csv(dat, "cc - hype_data.csv", row.names =F)
