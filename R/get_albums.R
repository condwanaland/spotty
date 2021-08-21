#' @export
get_saved_albums <- function(select_key_cols = TRUE,
                             rename_key_cols = TRUE){
  
  # if(!select_key_cols & rename_key_cols){
  #   stop("If select_key_cols is FALSE, then rename_key_cols must be as well")
  # }
  
  #base_url <- 'https://api.spotify.com/v1/me/albums'
  base_url <- album_url()
  
  total <- extract_total(base_url)
  
  res <- lapply(1:total, function(x){
    res <- run_query(base_url = base_url,
                     params = list(
                       limit = 50,
                       offset = (x - 1) * 50,
                       market = NULL
                     ))
  })
  
  
  res <- get_data(res)
  res_album <- get_album_data(res)
  
  res <- cbind(res, res_album)
  
  if (select_key_cols){
    res <- select_spotty_album_cols(res)
  }
  
  if(rename_key_cols){
    res <- rename_spotty_album_cols(res)
  }
  
  return(res)
  
}

get_album_data <- function(res){
  res_artists <- res$album.artists
  
  res <- lapply(res_artists, function(x){
    dat <- x[1, ]
  })
  res <- do.call(rbind, res)
}

select_spotty_album_cols <- function(dat){
  dat_cols <- dat[, c("album.name",
                      "name", #artist name
                      "added_at",
                      "album.popularity",
                      "album.release_date",
                      "album.release_date_precision",
                      "album.total_tracks",
                      "album.label",
                      "album.id",
                      "id", #artist id
                      "album.album_type"
  )]
  
  return(dat_cols)
}

rename_spotty_album_cols <- function(dat){
  #TODO: figure out how to mapply this
  dat <- renamer(dat, "album.name", "album_name")
  dat <- renamer(dat, "name", "artist_name")
  dat <- renamer(dat, "album.popularity", "album_popularity")
  dat <- renamer(dat, "album.id", "album_id")
  dat <- renamer(dat, "album.label", "album_label")
  dat <- renamer(dat, "album.release_date", "album_release_date")
  dat <- renamer(dat, "album.release_date_precision", "album_release_date_precision")
  dat <- renamer(dat, "track.album.total_tracks", "album_total_tracks")
  dat <- renamer(dat, "id", "artist_id")
  dat <- renamer(dat, "album.album_type", "album_type")
}

