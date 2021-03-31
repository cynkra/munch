# setup test --------------------------------------------------------------

all_data <- read_all_data()

source_years_summary <- all_data %>%
  count(year, mun_id_x, short_name_x) %>%
  count(year, n, name = "num_mun")

source_years_year <- source_years_summary$year
source_years_n <- source_years_summary$n

target_years_summary <- all_data %>%
  distinct(target_year, mun_id_y, short_name_y) %>%
  count(target_year, name = "num_mun")

source_combos <- all_data %>%
  distinct(year, mun_id_x, short_name_x) %>%
  rename(mun_id = mun_id_x, short_name = short_name_x)

target_combos <- all_data %>%
  distinct(target_year, mun_id_y, short_name_y) %>%
  rename(year = target_year, mun_id = mun_id_y, short_name = short_name_y)

# tests -------------------------------------------------------------------

test_that("internal consistency", {
  # test if each municipality appears once per csv per source year
  expect_identical(
    source_years_n,
    (source_years_year[length(source_years_year)] - source_years_year[1] + 1):1
  )

  # number of source municipalities equals number of target municipalities in each year
  expect_identical(
    source_years_summary$num_mun,
    target_years_summary$num_mun
  )

  # are all the names and municipality numbers equal for source and target municipalities in each year?
  expect_identical(
    anti_join(
      source_combos,
      target_combos,
      by = c("mun_id", "short_name", "year")
    ) %>%
      nrow(),
    0L
  )
})
