test_that("get_mun_hist() works", {
  expect_equal(
    get_mun_hist(4721, 1961, 2008),
    c(14073L, 11713L, 11917L)
  )

  expect_equal(
    get_mun_hist(4721, 1961, 2012),
    c(15443L, get_mun_hist(4721, 1961, 2008))
  )
})
