select_cols <- function(dat){
  UseMethod("select_cols")
}

select_cols.default <- function(dat){
  warning(paste("select_cols does not know how to handle object of class ", 
                class(x)))
  return(dat)
}

select_cols.spotty_track <- function(dat){
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

select_cols.spotty_album <- function(dat){
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