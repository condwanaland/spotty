test_that("url links are correct", {
  expect_equal(track_url(), 'https://api.spotify.com/v1/me/tracks/')
  expect_equal(album_url(), 'https://api.spotify.com/v1/me/albums')
})

