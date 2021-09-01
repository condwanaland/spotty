test_that("extract track total is numeric greater than 10", {
  skip_on_cran()
  skip_if_not(interactive())
  dat <- extract_total(track_url())
  expect_true(is.numeric(dat))
  expect_gt(dat, 10)
})

test_that("extract album total is numeric greater than 10", {
  skip_on_cran()
  skip_if_not(interactive())
  dat <- extract_total(album_url())
  expect_true(is.numeric(dat))
  expect_gt(dat, 10)
})

test_that("renamer function renames correctly", {
  iris2 <- renamer(iris, "Petal.Length", "petal_length")
  names(iris)[names(iris) == "Petal.Length"] <- "petal_length"
  
  expect_equal(iris, iris2)
})