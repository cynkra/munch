test_that("swc_get_merger_mapping_table() works", {
  # compare to known output
  expect_identical(
    swc_get_merger_mapping_table(2018, 2021, canton = "LU") %>%
      filter(mun_id_x != mun_id_y),
    tibble::tribble(
      ~year, ~mun_id_x, ~short_name_x, ~mun_id_y, ~short_name_y,
      2018,      1022L,      "Altwis",     1030L,   "Hitzkirch",
      2018,      1126L,  "Ebersecken",     1123L,  "Altishofen",
      2018,      1130L,     "Gettnau",     1151L,    "Willisau",
      2019,      1022L,      "Altwis",     1030L,   "Hitzkirch",
      2019,      1126L,  "Ebersecken",     1123L,  "Altishofen",
      2019,      1130L,     "Gettnau",     1151L,    "Willisau",
      2020,      1022L,      "Altwis",     1030L,   "Hitzkirch",
      2020,      1130L,     "Gettnau",     1151L,    "Willisau",
    )
  )

  expect_identical(
    swc_get_merger_mapping_table(2018, 2021, canton = "LU", type = "compact") %>%
      filter(mun_id_x != mun_id_y),
    tibble::tribble(
      ~year_from, ~year_to, ~mun_id_x, ~short_name_x, ~mun_id_y, ~short_name_y,
      2018, 2020, 1022L, "Altwis", 1030L, "Hitzkirch",
      2018, 2019, 1126L, "Ebersecken", 1123L, "Altishofen",
      2018, 2020, 1130L, "Gettnau", 1151L, "Willisau",
    )
  )

  expect_identical(
    swc_get_merger_mapping_table(2019, 2020, type = "flat") %>%
      filter(mun_id_x != mun_id_y),
    tibble::tribble(
      ~year, ~mun_id_x,         ~short_name_x,  ~mun_id_y, ~short_name_y,
      2019,       873L,      "Kirchenthurnen",       889L,     "Thurnen",
      2019,       874L,           "Lohnstorf",       889L,     "Thurnen",
      2019,       876L,        "Mühlethurnen",       889L,     "Thurnen",
      2019,       937L,        "Schwendibach",       939L, "Steffisburg",
      2019,       996L,          "Wolfisberg",       981L,  "Niederbipp",
      2019,      1126L,          "Ebersecken",      1123L,  "Altishofen",
      2019,      2111L, "Villaz-Saint-Pierre",      2117L,      "Villaz",
      2019,      2116L,          "La Folliaz",      2117L,      "Villaz",
      2019,      2185L,            "Corserey",      2237L,        "Prez",
      2019,      2213L,              "Noréaz",      2237L,        "Prez",
      2019,      2221L,    "Prez-vers-Noréaz",      2237L,        "Prez",
      2019,      3926L,            "Maladers",      3901L,        "Chur",
      2019,      4114L,      "Schinznach-Bad",      4095L,       "Brugg",
    )
  )

  expect_identical(
    swc_get_merger_mapping_table(2019, 2020, type = "compact") %>%
      filter(mun_id_x != mun_id_y),
    tibble::tribble(
      ~year_from, ~year_to, ~mun_id_x, ~short_name_x, ~mun_id_y, ~short_name_y,
      2019, 2019, 873L, "Kirchenthurnen", 889L, "Thurnen",
      2019, 2019, 874L, "Lohnstorf", 889L, "Thurnen",
      2019, 2019, 876L, "Mühlethurnen", 889L, "Thurnen",
      2019, 2019, 937L, "Schwendibach", 939L, "Steffisburg",
      2019, 2019, 996L, "Wolfisberg", 981L, "Niederbipp",
      2019, 2019, 1126L, "Ebersecken", 1123L, "Altishofen",
      2019, 2019, 2111L, "Villaz-Saint-Pierre", 2117L, "Villaz",
      2019, 2019, 2116L, "La Folliaz", 2117L, "Villaz",
      2019, 2019, 2185L, "Corserey", 2237L, "Prez",
      2019, 2019, 2213L, "Noréaz", 2237L, "Prez",
      2019, 2019, 2221L, "Prez-vers-Noréaz", 2237L, "Prez",
      2019, 2019, 3926L, "Maladers", 3901L, "Chur",
      2019, 2019, 4114L, "Schinznach-Bad", 4095L, "Brugg",
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
