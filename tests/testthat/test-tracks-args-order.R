test_that("Rename cols is not set when Select cols is not",{
  expect_error(get_saved_tracks(FALSE, TRUE), "If select_key_cols is FALSE")
})