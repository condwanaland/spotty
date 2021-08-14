get_scopes <- function(){
  all_scopes <- c("ugc-image-upload",
                  "user-read-recently-played",
                  "user-read-playback-state", 
                  "user-top-read",
                  "app-remote-control",
                  "playlist-modify-public", 
                  "user-modify-playback-state",
                  "playlist-modify-private",
                  "user-follow-modify", 
                  "user-read-currently-playing",
                  "user-follow-read",
                  "user-library-modify", 
                  "user-read-playback-position",
                  "playlist-read-private",
                  "user-read-email", 
                  "user-read-private",
                  "user-library-read",
                  "playlist-read-collaborative", 
                  "streaming")
  
  return(all_scopes)
}

