test_that("load_bfs_mun_list() works", {
  expect_identical(
    date_or_year_to_date("2019"),
    "01-01-2019"
  )

  expect_identical(
    date_or_year_to_date(2019),
    "01-01-2019"
  )

  expect_identical(
    date_or_year_to_date("2019-05-31"),
    "31-05-2019"
  )

  expect_identical(
    date_or_year_to_date(as.Date("2019-05-31")),
    "31-05-2019"
  )

  expect_identical(
    nrow(load_bfs_mun_list("2020-01-31")),
    2202L
  )

  expect_error(
    date_or_year_to_date(1848),
    class = swc_error("date_too_early")
  )

  # length of 4 implies it could be a year; needs to be a proper error,
  # if it cannot be made into an integer
  expect_error(
    date_or_year_to_date("abcd"),
    class = swc_error("not_date_or_year")
  )

  # length of 4 implies it could be a year
  expect_error(
    date_or_year_to_date("abcde"),
    class = swc_error("not_date_or_year")
  )
})
