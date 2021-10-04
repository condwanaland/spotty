renamer <- function(dat, from, to){
  names(dat)[names(dat) == from] <- to
  return(dat)
}

rename_cols <- function(dat){
  UseMethod("rename_cols")
}

rename_cols.default <- function(dat){
  warning(paste("rename_cols does not know how to handle object of class ", 
                class(dat)))
  return(dat)
}

rename_cols.spotty_track <- function(dat){
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


rename_cols.spotty_album <- function(dat){
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