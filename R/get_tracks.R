#' get_saved_tracks
#' 
#' Creates a dataframe of all of the tracks you have saved (liked)
#'
#' @param select_key_cols Boolean. Sets whether to select key columns of interest or alternatively to leave all columns as is. Defaults to TRUE
#' @param rename_key_cols Boolean. Sets whether to rename the key columns to more user-friendly names. Defaults to TRUE. Cannot be TRUE if `select_key_cols` is FALSE
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' client_id = Sys.setenv("SPOTIFY_CLIENT_ID"),
#' client_secret = Sys.setenv("SPOTIFY_CLIENT_SECRET")
#' dat <- get_saved_tracks()
#' }
get_saved_tracks <- function(select_key_cols = TRUE,
                             rename_key_cols = TRUE){
  
  if(!select_key_cols & rename_key_cols){
    stop("If select_key_cols is FALSE, then rename_key_cols must be as well")
  }
  
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
  
  
  res <- list_to_dataframe(res)
  class(res) <- append("spotty_track", class(res))
  
  #artists <- get_artist_data(res)
  artists <- extract_nested_data(res)

  res <- cbind(res, artists)
  # Figure out a way to not have to assign the class again here. cbind strips the custom class.
  class(res) <- append("spotty_track", class(res))
  #print(class(res))
  
  if(select_key_cols){
    res <- select_cols(res)
  }
  
  if(rename_key_cols){
    res <- rename_spotty_track_cols(res)
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

list_to_dataframe <- function(api_data){
  res <- lapply(api_data, function(x){
    as.data.frame(x$items)
  })
  
  res <- do.call(rbind, res)
  
  return(res)
}

rename_spotty_track_cols <- function(dat){
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
