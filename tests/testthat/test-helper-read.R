test_that("read-helpers work", {
  mut_data_download <- swc_read_data()

  expect_named(
    mut_data_download,
    c("canton", "district", "municipality", "metadata")
  )

  # FIXME: some more tests would be good, but maybe later
})
