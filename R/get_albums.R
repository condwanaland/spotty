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
  #artists <- get_artist_data(res)
  #res <- cbind(res, artists)
  
  # if(select_key_cols){
  #   res <- select_spotty_cols(res)
  # }
  # 
  # if(rename_key_cols){
  #   res <- rename_spotty_cols(res)
  # }
  
  return(res)
  
}