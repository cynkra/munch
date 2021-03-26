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


# Approach with asset number (approach used by swissdata as well)
# the asset number changes but the bfs_nr not.
# the asset number can be scraped when the bfs_nr is known.
# Disadvantage: only the current file can be downloaded
bfs_nr <- "dz-b-00.04-hgv-01"

bfs_home <- "https://www.bfs.admin.ch"

asset_page <- xml2::read_html(sprintf("%s/asset/de/%s", bfs_home, bfs_nr))

asset_number <- asset_page %>%
  html_text(bfs_nr) %>%
  str_extract("https://.*assets/.*/") %>%
  str_extract("[0-9]+")

pub_date <- asset_page %>%
  html_text(bfs_nr) %>%
  str_extract("Veröffentlicht am\n.*[0-9]+.[0-9]+.[0-9]+") %>%
  str_extract("[0-9]+.[0-9]+.[0-9]+")

download_link <- paste0("https://www.bfs.admin.ch/bfsstatic/dam/assets/",asset_number,"/master")

download.file(download_link, "download/test.zip")
unzip("download/test.zip", exdir = "download/test")

date <- paste0(year(Sys.Date()), "0101")
gde <- read_delim(
  paste0("download/test/01.1/",date,"_GDEHist_GDE.txt"),
  delim = "\t",
  col_names = FALSE,
  locale = locale(encoding = "latin1")
)

names(gde) <- c("mHistId", "dHistId", "cAbbreviation", "mId", "mLongName",
                "mShortName", "mEntryCode", "mStatus", "mAdmissionNumber",
                "mAdmissionCode", "mAdmissionDate", "mAbolitionNumber",
                "mAbolitionCode", "mAbolitionDate", "mDateOfChange")


mAbolitionCodes <- tribble(
  ~mAbolitionCode, ~mAbolitionMode,
  24,                "Reassignment to other d/c",
  29,                "Abolition",
  26,                "Change of area",
  23,                "Change of name (m)",
  27,                "Formal renumbering",
  22,                "Change of name (d)"
)

mAdmissionCodes <- tribble(
  ~mAdmissionCode,  ~mAdmissionMode,
  20,               "First-time registration",
  26,               "Change of area",
  24,               "Reassignment to other d/c",
  21,               "Creation",
  23,               "Change of name (m)",
  27,               "Formal renumbering",
  22,               "Change of name (d)"
)

mEntryCodes <- tribble(
  ~mEntryCode, ~mEntryMode,
  11,          "Municipality",
  13,          "Cantonal part of lake",
  12,          "Area not allocated to mun."
)


gde_asset <- gde %>%
  left_join(mEntryCodes, by = "mEntryCode") %>%
  left_join(mAdmissionCodes, by = "mAdmissionCode") %>%
  left_join(mAbolitionCodes, by = "mAbolitionCode") %>%
  select(
    mHistId,
    dHistId,
    cAbbreviation,
    mId,
    mLongName,
    mShortName,
    mEntryMode,
    mStatus,
    mAdmissionNumber,
    mAdmissionMode,
    mAdmissionDate,
    mAbolitionNumber,
    mAbolitionMode,
    mAbolitionDate,
    mDateOfChange
  ) %>%
  mutate_at(vars(matches("Date")), ~as.Date(., format("%d.%m.%Y")))

