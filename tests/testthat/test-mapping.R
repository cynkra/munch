context("mapping")

test_that("mapping returns integer columns", {
  expect_warning(mapping <- swcGetMapping(ids.from = 500, ids.to = 2875), NA)

  expect_is(mapping$mHistId.from, "integer")
  expect_is(mapping$mId.from, "integer")
  expect_is(mapping$mHistId.to, "integer")
  expect_is(mapping$mId.to, "integer")
})
