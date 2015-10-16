context("get")

test_that("swcGetData loads data as advertised", {
  swc <- swcGetData()
  expect_equal(names(swc), c("canton", "district", "municipality"))
  expect_true(all(unlist(plyr::llply(swc, is.data.frame))))
  expect_true(all(unlist(plyr::llply(swc, nrow)) > 0))

  expect_is(swc$municipality$mHistId, "integer")
  expect_is(swc$municipality$dHistId, "integer")
  expect_is(swc$municipality$mId, "integer")
  expect_is(swc$municipality$mAdmissionNumber, "integer")
  expect_is(swc$municipality$mAbolitionNumber, "integer")
})
