test_that("mapping returns integer columns", {
  mapping <- swc_get_mapping(ids_from = 500, ids_to = 2875)

  expect_type(mapping$mHistId.from, "integer")
  expect_type(mapping$mId.from, "integer")
  expect_null(mapping$mIdAsNumber.from)
  expect_type(mapping$mHistId.to, "integer")
  expect_type(mapping$mId.to, "integer")
  expect_null(mapping$mIdAsNumber.to)
})

test_that("mapping returns factors for factor input", {
  mapping <- swc_get_mapping(
    ids_from = factor(500),
    ids_to = factor(2875)
  )

  expect_type(mapping$mHistId.from, "integer")
  expect_true(class(mapping$mId.from) == "factor")
  expect_type(mapping$mIdAsNumber.from, "integer")
  expect_type(mapping$mHistId.to, "integer")
  expect_true(class(mapping$mId.to) == "factor")
  expect_type(mapping$mIdAsNumber.to, "integer")
})

test_that("deprecation error", {
  local_options(lifecycle_verbosity = "warning")
  expect_warning(mapping <- swcGetMapping(ids.from = 500, ids.to = 2875), regexp = "swcGetMapping")
  expect_identical(mapping, swc_get_mapping(ids_from = 500, ids_to = 2875))
})
