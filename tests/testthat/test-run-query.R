test_that("run_query tracks is expected shape", {
  skip_on_cran()
  skip_if_not(interactive())
  dat <- run_query(track_url(),
                   params = list(
    limit = 50,
    offset = 0,
    market = NULL))
  
  expect_equal(is.list(dat), TRUE)
  expect_equal(is.data.frame(dat$items), TRUE)
  expect_equal(nrow(dat$items), 50)
  expect_equal(is.integer(dat$total), TRUE)
  expect_equal(length(dat), 7)
  expect_equal(unname(lengths(dat)), c(1, 30, 1, 1, 1, 0, 1))
})

test_that("run_query album is expected shape", {
  skip_on_cran()
  skip_if_not(interactive())
  dat <- run_query(album_url(),
                   params = list(
                     limit = 50,
                     offset = 0,
                     market = NULL))
  
  expect_equal(is.list(dat), TRUE)
  expect_equal(is.data.frame(dat$items), TRUE)
  expect_equal(nrow(dat$items), 50)
  expect_equal(is.integer(dat$total), TRUE)
  expect_equal(length(dat), 7)
  expect_equal(unname(lengths(dat)), c(1, 26, 1, 1, 1, 0, 1))
})