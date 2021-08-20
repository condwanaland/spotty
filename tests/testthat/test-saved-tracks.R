# test_that("get_saved_tracks works", {
#   skip_on_cran()
#   expect_error(get_saved_tracks(), NA)
#   expect_gt(nrow(get_saved_tracks()), 500)
#   expect_equal(ncol(get_saved_tracks()), 11)
# })

test_that("get_saved_tracks works", {
  skip_on_cran()
  dat <- get_saved_tracks()
  #expect_error(get_saved_tracks(), NA)
  expect_gt(nrow(dat), 500)
  expect_equal(ncol(dat), 11)
})