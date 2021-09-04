extract_nested_data <- function(dat){
  UseMethod("extract_nested_data")
}

extract_nested_data.default <- function(dat){
  warning(paste("extract_nested_data does not know how to handle object of class ", 
                class(x)))
  return(dat)
}

extract_nested_data.spotty_track <- function(dat){
  artists <- dat$track.artists
  
  artists_reduced <- lapply(artists, function(x){
    x[1,]
  })
  
  bound_artists <- do.call(rbind, artists_reduced)
  
  return(bound_artists)
}

extract_nested_data.spotty_album <- function(dat){
  res_artists <- dat$album.artists
  
  res <- lapply(res_artists, function(x){
    dat <- x[1, ]
  })
  res <- do.call(rbind, res)
}