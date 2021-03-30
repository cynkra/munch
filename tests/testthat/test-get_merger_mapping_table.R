test_that("swc_get_merger_mapping_table() works", {
  # compare to known output
  expect_identical(
    swc_get_merger_mapping_table(2018, 2021, canton = "LU") %>%
      filter(mun_id_x != mun_id_y),
    tibble::tribble(
      ~year, ~mun_id_x, ~mun_id_y, ~short_name_x, ~short_name_y,
      2018,      1022,      1030,      "Altwis",   "Hitzkirch",
      2018,      1126,      1123,  "Ebersecken",  "Altishofen",
      2018,      1130,      1151,     "Gettnau",    "Willisau",
      2019,      1022,      1030,      "Altwis",   "Hitzkirch",
      2019,      1126,      1123,  "Ebersecken",  "Altishofen",
      2019,      1130,      1151,     "Gettnau",    "Willisau",
      2020,      1022,      1030,      "Altwis",   "Hitzkirch",
      2020,      1130,      1151,     "Gettnau",    "Willisau"
    )
  )

  # matching tables with same target year have to be identical,
  # if the one with the earlier source year is filtered to contain only those
  # year larger or equal to the source year of the other table
  expect_identical(
    swc_get_merger_mapping_table(2005, 2021) %>%
      filter(mun_id_x != mun_id_y, year >= 2018),
    swc_get_merger_mapping_table(2018, 2021) %>%
      filter(mun_id_x != mun_id_y)
  )
})
