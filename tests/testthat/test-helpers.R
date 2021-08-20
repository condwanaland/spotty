test_that("extract total is numeric greater than 10", {
  skip_on_cran()
  dat <- extract_total(track_url())
  expect_true(is.numeric(dat))
  expect_gt(dat, 10)
})