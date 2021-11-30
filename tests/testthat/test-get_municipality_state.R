# Just like `swc_get_merger_mapping_table()`, the result of `swc_get_municipality_state()`
# contains entries that are not municipalities, see `tests/testthat/test-external-consistency.R`

test_that("swc_get_municipality_state() works", {
  # some tests are already conducted in `test-external-consistency.R`

  expect_identical(
    swc_get_municipality_state(2005, canton = "LU") %>%
      pull(mun_id),
    load_bfs_mun_list(2005, canton = "LU") %>%
      pull(mun_id)
  )

  expect_identical(
    swc_get_municipality_state(2018, canton = "LU") %>%
      pull(mun_id),
    load_bfs_mun_list(2018, canton = "LU") %>%
      pull(mun_id)
  )
})
