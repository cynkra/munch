context("check")

test_that("swcCheckData loads data as advertised", {
  expect_that(check <- swcCheckData(), not(throws_error()))
  expect_equal(names(check), c("statesWithNonUniqueAdmissionNumbers", "stateSequencesWithDecreasingDate"))
  expect_true(all(unlist(llply(check, is.data.frame))))
  expect_true(all(unlist(llply(check, nrow)) > 0))
})
