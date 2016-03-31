context("check")

test_that("swcCheckData loads data as advertised", {
  check <- swcCheckData()
  expect_equal(names(check), c("statesWithNonUniqueAdmissionNumbers", "stateSequencesWithDecreasingDate"))
  expect_true(all(unlist(plyr::llply(check, is.data.frame))))
  expect_true(all(unlist(plyr::llply(check, nrow)) > 0))
})
