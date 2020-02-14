context("mapping")

test_that("mapping returns integer columns", {
  expect_warning(mapping <- swc_get_mapping(ids.from = 500, ids.to = 2875), NA)
  expect_warning(mapping <- swcGetMapping(ids.from = 500, ids.to = 2875))

  expect_is(mapping$mHistId.from, "integer")
  expect_is(mapping$mId.from, "integer")
  expect_null(mapping$mIdAsNumber.from)
  expect_is(mapping$mHistId.to, "integer")
  expect_is(mapping$mId.to, "integer")
  expect_null(mapping$mIdAsNumber.to)
})

test_that("mapping returns factors for factor input", {
  expect_warning(mapping <- swc_get_mapping(
    ids.from = factor(500),
    ids.to = factor(2875)
  ), NA)
  expect_warning(mapping <- swcGetMapping(
    ids.from = factor(500),
    ids.to = factor(2875)
  ))

  expect_is(mapping$mHistId.from, "integer")
  expect_is(mapping$mId.from, "factor")
  expect_is(mapping$mIdAsNumber.from, "integer")
  expect_is(mapping$mHistId.to, "integer")
  expect_is(mapping$mId.to, "factor")
  expect_is(mapping$mIdAsNumber.to, "integer")
})
