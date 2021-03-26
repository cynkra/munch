library(jsonlite)
library(RCurl)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
library(xml2)
library(rvest)
library(lubridates)

# API
# not the whole data content (admission date and abolition date are missing)
# only downloadable as whole character string
download_string <- paste0("https://sms.bfs.admin.ch/WcfBFSSpecificService.svc/",
                          "AnonymousRest/communes/mutations?startPeriod=",
                          "01-01-1960&endPeriod=01-01-2021&includeTerritoryExchange=true&format=csv")

data <- getURL(download_string)

split_data <- strsplit(test, "\r\n")
data_tibble <- tibble(
  mutations = test4[[1]]
) %>%
  mutate(mutations = str_replace_all(mutations, '\"', ''))

names <- strsplit(first(data_tibble$mutations), ",")[[1]]

gde_api <- data_tibble %>%
  separate(mutations,into = c(names), sep = ",") %>%
  slice(-1) %>%
  mutate_at(vars(mHistId, dHistId, mId, mStatus, mAdmissionNumber, mAbolitionNumber), as.numeric) %>%
  mutate_at(vars(matches(Date)), as.Date)


