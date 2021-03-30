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

  expect_identical(
    swc_get_merger_mapping_table(2018, 2021, canton = "LU", type = "compact") %>%
      filter(mun_id_x != mun_id_y),
    tibble::tribble(
      ~year_from, ~year_to, ~mun_id_x, ~short_name_x, ~mun_id_y, ~short_name_y,
      2018,     2020,      1022,      "Altwis",      1030,   "Hitzkirch",
      2018,     2019,      1126,  "Ebersecken",      1123,  "Altishofen",
      2018,     2020,      1130,     "Gettnau",      1151,    "Willisau"
    )
  )

  expect_identical(
    swc_get_merger_mapping_table(2019, 2020, type = "flat") %>%
      filter(mun_id_x != mun_id_y),
    tibble::tribble(
      ~year, ~mun_id_x, ~mun_id_y,         ~short_name_x, ~short_name_y,
      2019,       873,       889,      "Kirchenthurnen",     "Thurnen",
      2019,       874,       889,           "Lohnstorf",     "Thurnen",
      2019,       876,       889,        "Mühlethurnen",     "Thurnen",
      2019,       937,       939,        "Schwendibach", "Steffisburg",
      2019,       996,       981,          "Wolfisberg",  "Niederbipp",
      2019,      1126,      1123,          "Ebersecken",  "Altishofen",
      2019,      2111,      2117, "Villaz-Saint-Pierre",      "Villaz",
      2019,      2116,      2117,          "La Folliaz",      "Villaz",
      2019,      2185,      2237,            "Corserey",        "Prez",
      2019,      2213,      2237,              "Noréaz",        "Prez",
      2019,      2221,      2237,    "Prez-vers-Noréaz",        "Prez",
      2019,      3926,      3901,            "Maladers",        "Chur",
      2019,      4114,      4095,      "Schinznach-Bad",       "Brugg"
    )
  )

  expect_identical(
    swc_get_merger_mapping_table(2019, 2020, type = "compact") %>%
      filter(mun_id_x != mun_id_y),
    tibble::tribble(
      ~year_from, ~year_to, ~mun_id_x,         ~short_name_x, ~mun_id_y, ~short_name_y,
      2019,     2019,       873,      "Kirchenthurnen",       889,     "Thurnen",
      2019,     2019,       874,           "Lohnstorf",       889,     "Thurnen",
      2019,     2019,       876,        "Mühlethurnen",       889,     "Thurnen",
      2019,     2019,       937,        "Schwendibach",       939, "Steffisburg",
      2019,     2019,       996,          "Wolfisberg",       981,  "Niederbipp",
      2019,     2019,      1126,          "Ebersecken",      1123,  "Altishofen",
      2019,     2019,      2111, "Villaz-Saint-Pierre",      2117,      "Villaz",
      2019,     2019,      2116,          "La Folliaz",      2117,      "Villaz",
      2019,     2019,      2185,            "Corserey",      2237,        "Prez",
      2019,     2019,      2213,              "Noréaz",      2237,        "Prez",
      2019,     2019,      2221,    "Prez-vers-Noréaz",      2237,        "Prez",
      2019,     2019,      3926,            "Maladers",      3901,        "Chur",
      2019,     2019,      4114,      "Schinznach-Bad",      4095,       "Brugg"
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
