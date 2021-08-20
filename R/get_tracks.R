#' @export
get_saved_tracks <- function(select_key_cols = TRUE,
                             rename_key_cols = TRUE){
  
  if(!select_key_cols & rename_key_cols){
    stop("If select_key_cols is FALSE, then rename_key_cols must be as well")
  }
  
  #base_url <- 'https://api.spotify.com/v1/me/tracks/'
  base_url <- track_url()
  
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
  artists <- get_artist_data(res)
  res <- cbind(res, artists)
  
  if(select_key_cols){
    res <- select_spotty_cols(res)
  }
  
  if(rename_key_cols){
    res <- rename_spotty_cols(res)
  }
  
  return(res)
  
}

run_query <- function(base_url, params){
  res <- httr::RETRY('GET', base_url, query = params,
                     httr::config(token = get_spotify_authorization_code()),
                     encode = 'json',
                     terminate_on = c(401, 403, 404))
  
  res <- jsonlite::fromJSON(httr::content(res, as = 'text', encoding = 'UTF-8'),
                            flatten = TRUE, simplifyDataFrame = TRUE)
  
  return(res)
}

extract_total <- function(base_url){
  
  metadata <- run_query(base_url, params = list(
    limit = 1,
    offset = 0,
    market = NULL
  ))
  total <- ceiling(metadata[['total']] / 50)
  
  return(total)
}

get_data <- function(api_data){
  res <- lapply(api_data, function(x){
    as.data.frame(x$items)
  })
  
  res <- do.call(rbind, res)
  
  return(res)
}

select_spotty_cols <- function(dat){
  dat_cols <- dat[, c("track.name",
                      "track.album.name",
                      "name",
                      "added_at",
                      "track.duration_ms",
                      "track.popularity",
                      "track.album.release_date",
                      "track.album.total_tracks",
                      "track.id",
                      "track.album.id",
                      "id"
  )]
  
  return(dat_cols)
}

rename_spotty_cols <- function(dat){
  #TODO: figure out how to mapply this
  dat <- renamer(dat, "track.duration_ms", "track_duration_ms")
  dat <- renamer(dat, "track.id", "track_id")
  dat <- renamer(dat, "track.name", "track_name")
  dat <- renamer(dat, "track.popularity", "track_popularity")
  dat <- renamer(dat, "track.album.id", "album_id")
  dat <- renamer(dat, "track.album.name", "album_name")
  dat <- renamer(dat, "track.album.release_date", "album_name")
  dat <- renamer(dat, "track.album.total_tracks", "album_total_tracks")
  dat <- renamer(dat, "id", "artist_id")
  dat <- renamer(dat, "name", "artist_name")
}



renamer <- function(dat, from, to){
  names(dat)[names(dat) == from] <- to
  return(dat)
}

get_artist_data <- function(bound_data){
  artists <- bound_data$track.artists
  
  artists_reduced <- lapply(artists, function(x){
    x[1,]
  })
  
  bound_artists <- do.call(rbind, artists_reduced)
  
  return(bound_artists)
}

