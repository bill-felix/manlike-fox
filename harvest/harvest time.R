 #  HARVEST #
 # H ARVEST #
## HA RVEST ##
 # HAR VEST #
 # HARV EST #
 # HARVE ST #
 # HARVEST  #

library(rvest)
library(dplyr)
library(XML)


# choose where you want to harvest
twc_url <- "http://www.timewarnercable.com/en/plans-packages/internet/internet-service-plans.html"

# cinbell_url <- "http://www.cincinnatibell.com/internet/"

# create a value to reference for variable creation
twc_content <- twc_url %>%
  html() %>%
  html_nodes("*")


# vignette("selectorgadget") # is useful for finding css something or others
# create the plan variable
twc_plans <- twc_content %>%
  html_node(".medium") %>%
  html_text()









##### EXAMPLE #####
# Inspired by
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html

library(rvest)

url <- "http://www.tripadvisor.com/Hotel_Review-g37209-d1762915-Reviews-JW_Marriott_Indianapolis-Indianapolis_Indiana.html"

reviews <- url %>%
  html() %>%
  html_nodes("#REVIEWS .innerBubble")

id <- reviews %>%
  html_node(".quote a") %>%
  html_attr("id")

quote <- reviews %>%
  html_node(".quote span") %>%
  html_text()

rating <- reviews %>%
  html_node(".rating .rating_s_fill") %>%
  html_attr("alt") %>%
  gsub(" of 5 stars", "", .) %>%
  as.integer()

date <- reviews %>%
  html_node(".rating .ratingDate") %>%
  html_attr("title") %>%
  strptime("%b %d, %Y") %>%
  as.POSIXct()

review <- reviews %>%
  html_node(".entry .partial_entry") %>%
  html_text()

data.frame(id, quote, rating, date, review, stringsAsFactors = FALSE) %>% View()