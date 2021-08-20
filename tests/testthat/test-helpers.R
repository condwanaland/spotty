test_that("extract track total is numeric greater than 10", {
  skip_on_cran()
  dat <- extract_total(track_url())
  expect_true(is.numeric(dat))
  expect_gt(dat, 10)
})

test_that("extract album total is numeric greater than 10", {
  skip_on_cran()
  dat <- extract_total(album_url())
  expect_true(is.numeric(dat))
  expect_gt(dat, 10)
})