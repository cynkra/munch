library(tidyverse)
library(here)

read_mun_csv <- function(file) {
  target_year <- sub(".csv", "", basename(file))

  df <- read.csv(file) %>%
    mutate(target_year = target_year)

  return(df)
}

all_files <- list.files(here::here("inst/csv/flat"))

file_paths <- paste0(here::here("inst/csv/flat/"), all_files)

all_files <-
  file_paths %>%
  map_df(~ read_mun_csv(.)) %>%
  as_tibble()

all_files %>%
  count(year, mun_id_x) %>%
  nest(data = -c(year, n)) %>%
  arrange(year, desc(n)) %>%
  identity() -> source_years

source_years %>%
  group_by(year) %>%
  filter(n != max(n)) %>%
  ungroup() %>%
  unnest(data)

all_files %>%
  distinct(target_year, mun_id_y) %>%
  count(target_year, mun_id_y) %>%
  nest(data = -c(target_year, n)) %>%
  arrange(target_year, desc(n)) %>%
  identity() -> target_years

target_years %>%
  group_by(target_year) %>%
  filter(n != max(n)) %>%
  ungroup()
