library(dplyr)
library(purrr)
library(here)
library(tidyr)

read_mun_csv <- function(file){
  target_year <- sub(".csv", "", basename(file))

  df <- read.csv(file) %>%
    mutate(target_year = target_year)

  return(df)
}

all_files <- list.files(here::here("inst/csv/flat"))

file_paths <- paste0(here::here("inst/csv/flat/"), all_files)

all_files <- file_paths %>% map_df(~read_mun_csv(.))


check <- function(source_year, data){
  data_source_year <- data %>%
    filter(year == source_year)

  source_year_data <- data_source_year %>%
    distinct(target_year, mun_id_x, short_name_x) %>%
    group_by(target_year) %>%
    summarize(n = n()) %>%
    mutate(source_year = source_year)
}

all_source_years <- unique(all_files$year)

checked_data <- all_source_years %>%
  map_df(~check(., all_files)) %>%
  pivot_wider(names_from = source_year, values_from = n)



