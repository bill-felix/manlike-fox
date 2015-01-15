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


options(max.print = 100) ## limit printed results


# import data (hype for now)

rdate <- function(n, start, end) {
     st <- as.POSIXct(as.Date(start))
     et <- as.POSIXct(as.Date(end))
     dt <- as.numeric(difftime(et,st,unit="sec"))
     ev <- runif(n, min = 1.1, max = dt)
     rt <- st + ev
}

dat1 <- data.frame(id = sample(8000:9999, 100),
                  u_long = rnorm(100, mean = 84.5352946, sd = .00005),
                  u_lat = rnorm(100, mean = 39.1651805, sd = .00005),
                  twc = round(runif(100, min = 45, max = 155), digits =2),
                  twc_wkcc = round(mean(dat1$twc), digits =2),
                  cinbell = round(runif(100, min = 55, max = 95), digits =2),
                  cinbell_wkcc = round(mean(dat1$cinbell), digits =2),
                  comcast = round(runif(100, min = 75, max = 80), digits =2),
                  comcast_wkcc = round(mean(dat1$comcast), digits =2),
                  date = rdate(100, "2014/12/28", "2015/01/03"))

dat2 <- data.frame(id = sample(8000:9999, 100),
                  u_long = rnorm(100, mean = 84.5352946, sd = .00005),
                  u_lat = rnorm(100, mean = 39.1651805, sd = .00005),
                  twc = round(runif(100, min = 45, max = 155), digits =2),
                  twc_wkcc = round(mean(dat2$twc), digits =2),
                  cinbell = round(runif(100, min = 55, max = 95), digits =2),
                  cinbell_wkcc = round(mean(dat2$cinbell), digits =2),
                  comcast = round(runif(100, min = 75, max = 80), digits =2),
                  comcast_wkcc = round(mean(dat2$comcast), digits =2),
                  date = rdate(100, "2015/01/04", "2015/01/10"))

dat3 <- data.frame(id = sample(8000:9999, 100),
                  u_long = rnorm(100, mean = 84.5352946, sd = .00005),
                  u_lat = rnorm(100, mean = 39.1651805, sd = .00005),
                  twc = round(runif(100, min = 45, max = 155), digits =2),
                  twc_wkcc = round(mean(dat3$twc), digits =2),
                  cinbell = round(runif(100, min = 55, max = 95), digits =2),
                  cinbell_wkcc = round(mean(dat3$cinbell), digits =2),
                  comcast = round(runif(100, min = 75, max = 80), digits =2),
                  comcast_wkcc = round(mean(dat3$comcast), digits =2),
                  date = rdate(100, "2015/01/11", "2015/01/17"))

dat4 <- data.frame(id = sample(8000:9999, 100),
                  u_long = rnorm(100, mean = 84.5352946, sd = .00005),
                  u_lat = rnorm(100, mean = 39.1651805, sd = .00005),
                  twc = round(runif(100, min = 45, max = 155), digits =2),
                  twc_wkcc = round(mean(dat4$twc), digits =2),
                  cinbell = round(runif(100, min = 55, max = 95), digits =2),
                  cinbell_wkcc = round(mean(dat4$cinbell), digits =2),
                  comcast = round(runif(100, min = 75, max = 80), digits =2),
                  comcast_wkcc = round(mean(dat4$comcast), digits =2),
                  date = rdate(100, "2015/01/18", "2015/01/24"))
# rbind
dat <- rbind(dat1,dat2,dat3,dat4)

# lubridate functions for extraction
dat$year <- year(dat$date)
dat$quarter <- quarter(dat$date)
dat$month <- month(dat$date)
dat$day <- day(dat$date)

# ggplot2 #

# Prices on given days?
tw_plot <- ggplot(dat, aes(x=day, y=twc)) + geom_point(color = "light blue")
cb_plot <- ggplot(dat, aes(x=day, y=cinbell)) + geom_point(color = "red")
com_plot <- ggplot(dat, aes(x=day, y=comcast)) + geom_point(color = "yellow")

# Which months have high opens?
ggplot(dat, aes(x=month, y=total_opens)) + geom_point()

# Boxplot of monthly avg_opens_per_person (plus alts)
g <- ggplot(dat_d, aes(x=as.factor(month), y=avg_opens_per_person))
g + geom_boxplot(fill="orange")
g + geom_jitter(alpha = .3,position = position_jitter(width = .2))
g + geom_violin(alpha=0.5, color="blue")
g + geom_violin(alpha=0.5, color="gray") + 
  geom_jitter(alpha = .7, aes(color = "red"), position = position_jitter(width = .2)) + 
  coord_flip()

#######################################################################

providers <- c("twc", "cinbell", "comcast")

alt_dat1 <- data.frame(id = sample(8000:9999, 100),
                  u_long = rnorm(100, mean = 84.5352946, sd = .00005),
                  u_lat = rnorm(100, mean = 39.1651805, sd = .00005),
                  provider = sample(providers, 100, replace =T),
                  price = runif(100, min = 45, max = 145),
#                   tw_mean = ifelse(alt_dat1$provider == "twc", mean(alt_dat1[alt_dat1$provider == "twc",5], rm.na =T), 0),
#                   cb_mean = ifelse(alt_dat1$provider == "cinbell", mean(alt_dat1[alt_dat1$provider == "cinbell",5], rm.na =T), 0),
#                   com_mean = ifelse(alt_dat1$provider == "comcast", mean(alt_dat1[alt_dat1$provider == "comcast",5], rm.na =T), 0),
                  date = rdate(100, "2014/12/28", "2015/01/03"))
alt_dat2 <- data.frame(id = sample(8000:9999, 100),
                  u_long = rnorm(100, mean = 84.5352946, sd = .00005),
                  u_lat = rnorm(100, mean = 39.1651805, sd = .00005),
                  provider = sample(providers, 100, replace =T),
                  price = runif(100, min = 45, max = 145),
#                   tw_mean = ifelse(alt_dat2$provider == "twc", mean(alt_dat2[alt_dat2$provider == "twc",5], rm.na =T), 0),
#                   cb_mean = ifelse(alt_dat2$provider == "cinbell", mean(alt_dat2[alt_dat2$provider == "cinbell",5], rm.na =T), 0),
#                   com_mean = ifelse(alt_dat2$provider == "comcast", mean(alt_dat2[alt_dat2$provider == "comcast",5], rm.na =T), 0),
                  date = rdate(100, "2015/01/04", "2015/01/10"))
alt_dat3 <- data.frame(id = sample(8000:9999, 100),
                  u_long = rnorm(100, mean = 84.5352946, sd = .00005),
                  u_lat = rnorm(100, mean = 39.1651805, sd = .00005),
                  provider = sample(providers, 100, replace =T),
                  price = runif(100, min = 45, max = 145),
#                   tw_mean = ifelse(alt_dat3$provider == "twc", mean(alt_dat3[alt_dat3$provider == "twc",5], rm.na =T), 0),
#                   cb_mean = ifelse(alt_dat3$provider == "cinbell", mean(alt_dat3[alt_dat3$provider == "cinbell",5], rm.na =T), 0),
#                   com_mean = ifelse(alt_dat3$provider == "comcast", mean(alt_dat3[alt_dat3$provider == "comcast",5], rm.na =T), 0),
                  date = rdate(100, "2015/01/11", "2015/01/17"))
alt_dat4 <- data.frame(id = sample(8000:9999, 100),
                  u_long = rnorm(100, mean = 84.5352946, sd = .00005),
                  u_lat = rnorm(100, mean = 39.1651805, sd = .00005),
                  provider = sample(providers, 100, replace =T),
                  price = runif(100, min = 45, max = 145),
#                   tw_mean = ifelse(alt_dat4$provider == "twc", mean(alt_dat4[alt_dat4$provider == "twc",5], rm.na =T), 0),
#                   cb_mean = ifelse(alt_dat4$provider == "cinbell", mean(alt_dat4[alt_dat4$provider == "cinbell",5], rm.na =T), 0),
#                   com_mean = ifelse(alt_dat4$provider == "comcast", mean(alt_dat4[alt_dat4$provider == "comcast",5], rm.na =T), 0),
                  date = rdate(100, "2015/01/18", "2015/01/24"))

# rbind
alt_dat <- rbind(alt_dat1,alt_dat2,alt_dat3,alt_dat4)


