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
  h <- arrange(h, date) ## order observations by date
  h <- ddply(h, .(provider, zip), function(x) {x$lowest <- cummin(x$price); return(x)}) ## calculate lowest price to date
  h <- ddply(h, .(provider, zip), function(x) {x$average <- round(cumsum(x$price)/seq_along(x$price), digits =2); return(x)}) ## calculate avg price to date
  ## h <- ddply(h, .(provider,zip), function(x) {x$avg_price <- rollmean(x$price, 10, na.pad=TRUE, align = "right"); return(x)}) ## rolling avg by last 10 obvs
  h <- ddply(h, .(provider, zip), function(x) {x$highest <- cummax(x$price); return(x)}) ## calculate highest price to date 
  h %<>%
    mutate(thmbdown = ifelse(h$average < h$price, 1, 0))
}

populate_hype_data <- function(x,n) {
  x <- data.frame(id = sample(50000000:99999999, n),
                  u_long = rnorm(n, mean = 84.5352946, sd = .005),
                  u_lat = rnorm(n, mean = 39.1651805, sd = .005),
                  zip = sample(zips, n, replace =T),
                  date = rdate(n, "2014/01/01", "2015/12/31"),
                  provider = sample(providers, n, replace =T),
                  price = round(runif(n, min = 45, max = 145), digits =2))
}

set.seed(6)
dat <- NULL
providers <- c("twc", "cinbell", "comcast")
zips <- c("45223", "45202", "45206", "45201", "45239", "45205", "45207", "45225", "45217", "45213", "45214", "45216")
general_price <- round(runif(1000, min = 40, max = 160), digits = 2)

dat <- populate_hype_data(dat,1000000)

# artificially discount or raise providers
set.seed(80)
artificial <- rnorm(500, mean = 1, sd = .035)
counter <- 20
repeat {
  dat %<>%
  mutate(price = ifelse(zip == sample(zips, 1) & provider == sample(providers, 1), price * sample(artificial, 1), price * 1))
  print(round(mean(dat$price)), digits = 2)
  counter <- counter - 1
  if(counter < 0)
  break;   
}

# submit function simulates the submit button and returns results
dat <- submit(dat)

# lubridate functions for extraction
dat$year <- year(dat$date)
dat$quarter <- quarter(dat$date)
dat$month <- month(dat$date)
dat$day <- day(dat$date)

############################## ggplot2 ###############################################

# Prices on given days?
price_per_day <- ggplot(dat, aes(x=day, y=price))
prov_avg_price <- ggplot(dat, aes(x=provider, y=average))
zip_avg_price <- ggplot(dat, aes(x=zip, y=average))

# Price per date
ggplot(dat, aes(x=date, y=price)) + geom_point()

# Boxplot of monthly avg_opens_per_person (plus alts)
g <- ggplot(dat_d, aes(x=as.factor(month), y=avg_opens_per_person))
g + geom_boxplot(fill="orange")
price_per_day + geom_boxplot(fill="orange")
prov_avg_price + geom_boxplot(fill="orange")
zip_avg_price + geom_boxplot(fill="orange")
g + geom_jitter(alpha = .3,position = position_jitter(width = .2))
price_per_day + geom_jitter(alpha = .3,position = position_jitter(width = .2))
prov_avg_price + geom_jitter(alpha = .3,position = position_jitter(width = .2))
zip_avg_price + geom_jitter(alpha = .3,position = position_jitter(width = .2))
g + geom_violin(alpha=0.5, color="blue")
price_per_day + geom_violin(alpha=0.5, color="blue")
prov_avg_price + geom_violin(alpha=0.5, color="blue")
zip_avg_price + geom_violin(alpha=0.5, color="blue")
g + geom_violin(alpha=0.5, color="gray") + 
  geom_jitter(alpha = .7, aes(color = "red"), position = position_jitter(width = .2)) + 
  coord_flip()
price_per_day + geom_violin(alpha=0.5, color="gray") + 
  geom_jitter(alpha = .7, aes(color = "red"), position = position_jitter(width = .2)) + 
  coord_flip()
prov_avg_price + geom_violin(alpha=0.5, color="gray") + 
  geom_jitter(alpha = .7, aes(color = "red"), position = position_jitter(width = .2)) + 
  coord_flip()
zip_avg_price + geom_violin(alpha=0.5, color="gray") + 
  geom_jitter(alpha = .7, aes(color = "red"), position = position_jitter(width = .2)) + 
  coord_flip()

#######################################################################


# Same plot as above, declaring only the data frame in ggplot().
# Note how the x and y aesthetics must now be declared in
# each geom_point() layer.
ggplot(dat) +
   geom_violin(aes(x = provider, y = price), aplha=0.5, color ="gray") +
   geom_jitter(data = dat3, aes(x = provider, y = price, color = "red"), alpha = .7, position = position_jitter(width = .2)) +
   geom_jitter(data = dat4, aes(x = provider, y = price, color = "blue"), alpha = .7, position = position_jitter(width = .2))

ggplot(dat) +
   geom_violin(aes(x = zip, y = price), aplha=0.5, color ="gray") +
   geom_jitter(data = dat3, aes(x = zip, y = price, color = "red"), alpha = .7, position = position_jitter(width = .2)) +
   geom_jitter(data = dat4, aes(x = zip, y = price, color = "blue"), alpha = .7, position = position_jitter(width = .2))

ggplot(dat) +
   geom_violin(aes(x = zip, y = price, aplha=0.5, color ="gray", group = provider)) +
   geom_jitter(data = dat3, aes(x = zip, y = price, color = "red"), alpha = .7, position = position_jitter(width = .2)) +
   geom_jitter(data = dat3, aes(x = zip, y = price, color = "blue"), alpha = .7, position = position_jitter(width = .2))

ggplot(dat, aes(y = price, x = zip, fill = provider)) + geom_boxplot()


###########################################

write.csv(dat, "cc - hype_data.csv", row.names =F)



# dat1 <- data.frame(id = sample(50000:99999, 10000),
#                   u_long = rnorm(10000, mean = 84.5352946, sd = .00005),
#                   u_lat = rnorm(10000, mean = 39.1651805, sd = .00005),
#                   zip = sample(zips, 10000, replace =T),
#                   date = rdate(10000, "2014/12/28", "2015/01/03"),
#                   provider = sample(providers, 10000, replace =T),
#                   price = round(runif(10000, min = 45, max = 145), digits =2))
# dat2 <- data.frame(id = sample(50000:99999, 10000),
#                   u_long = rnorm(10000, mean = 84.5352946, sd = .00005),
#                   u_lat = rnorm(10000, mean = 39.1651805, sd = .00005),
#                   zip = sample(zips, 10000, replace =T),
#                   provider = sample(providers, 10000, replace =T),
#                   price = runif(10000, min = 55, max = 135),
#                   date = rdate(10000, "2015/01/04", "2015/01/10"))
# dat3 <- data.frame(id = sample(50000:99999, 10000),
#                   u_long = rnorm(10000, mean = 84.5352946, sd = .00005),
#                   u_lat = rnorm(10000, mean = 39.1651805, sd = .00005),
#                   zip = sample(zips, 10000, replace =T),
#                   provider = sample(providers, 10000, replace =T),
#                   price = runif(10000, min = 55, max = 120),
#                   date = rdate(10000, "2015/01/11", "2015/01/17"))
# dat4 <- data.frame(id = sample(50000:99999, 10000),
#                   u_long = rnorm(10000, mean = 84.5352946, sd = .00005),
#                   u_lat = rnorm(10000, mean = 39.1651805, sd = .00005),
#                   zip = sample(zips, 10000, replace =T),
#                   provider = sample(providers, 10000, replace =T),
#                   price = runif(10000, min = 65, max = 105),
#                   date = rdate(10000, "2015/01/18", "2015/01/24"))
# dat5 <- data.frame(id = sample(50000:99999, 10000),
#                   u_long = rnorm(10000, mean = 84.5352946, sd = .00005),
#                   u_lat = rnorm(10000, mean = 39.1651805, sd = .00005),
#                   zip = sample(zips, 10000, replace =T),
#                   date = rdate(10000, "2015/01/25", "2015/01/31"),
#                   provider = sample(providers, 10000, replace =T),
#                   price = round(runif(10000, min = 45, max = 140), digits =2))
# dat6 <- data.frame(id = sample(50000:99999, 10000),
#                   u_long = rnorm(10000, mean = 84.5352946, sd = .00005),
#                   u_lat = rnorm(10000, mean = 39.1651805, sd = .00005),
#                   zip = sample(zips, 10000, replace =T),
#                   provider = sample(providers, 10000, replace =T),
#                   price = runif(10000, min = 65, max = 155),
#                   date = rdate(10000, "2015/02/01", "2015/02/07"))
# dat7 <- data.frame(id = sample(50000:99999, 10000),
#                   u_long = rnorm(10000, mean = 84.5352946, sd = .00005),
#                   u_lat = rnorm(10000, mean = 39.1651805, sd = .00005),
#                   zip = sample(zips, 10000, replace =T),
#                   provider = sample(providers, 10000, replace =T),
#                   price = runif(10000, min = 55, max = 130),
#                   date = rdate(10000, "2015/02/08", "2015/02/14"))
# dat8 <- data.frame(id = sample(50000:99999, 10000),
#                   u_long = rnorm(10000, mean = 84.5352946, sd = .00005),
#                   u_lat = rnorm(10000, mean = 39.1651805, sd = .00005),
#                   zip = sample(zips, 10000, replace =T),
#                   provider = sample(providers, 10000, replace =T),
#                   price = runif(10000, min = 75, max = 100),
#                   date = rdate(10000, "2015/02/15", "2015/02/21"))

# # rbind
# dat <- rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8)