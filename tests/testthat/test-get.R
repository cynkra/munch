context("get")

test_that("swcGetData loads data as advertised", {
  swc <- swcGetData()
  expect_equal(names(swc), c("canton", "district", "municipality"))
  expect_true(all(unlist(plyr::llply(swc, is.data.frame))))
  expect_true(all(unlist(plyr::llply(swc, nrow)) > 0))
})
