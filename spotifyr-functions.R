#' Get Spotify Authorization Code
#'
#' This function is copied verbatim from the `spotifyr` package by [Charlie Thompson](https://www.rcharlie.com/spotifyr/index.html). It is being used in accordance with its MIT License. All copyright is retained by C Thompson as follows
#' YEAR: 2017
#' COPYRIGHT HOLDER: Charlie Thompson
#' 
#' This function creates a Spotify authorization code.
#' See \code{httr::\link[httr]{oauth2.0_token}}.
#'
#' @param client_id Defaults to System Envioronment variable "SPOTIFY_CLIENT_ID"
#' @param client_secret Defaults to System Envioronment variable "SPOTIFY_CLIENT_SECRET"
#' @param scope Space delimited string of spotify scopes,
#' found here: https://developer.spotify.com/documentation/general/guides/scopes/.
#' All scopes are selected by default
#' @export
#' @return The Spotify Web API Token2.0 reference class object (see
#'  \code{httr::\link[httr]{oauth2.0_token}}), or an error message.
#' @family authentication functions
#' @importFrom httr oauth2.0_token oauth_endpoint
#' @importFrom purrr safely
#' @examples
#' \donttest{
#' authorization <- get_spotify_authorization_code()
#' }

get_spotify_authorization_code <- function(
  client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
  client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
  scope = get_scopes()
) {
  
  endpoint <- oauth_endpoint(authorize = 'https://accounts.spotify.com/authorize',
                             access = 'https://accounts.spotify.com/api/token')
  
  app <- oauth_app('spotty', client_id, client_secret)
  
  token <- purrr::safely(.f=oauth2.0_token)(
    endpoint = endpoint,
    app = app,
    scope = scope)
  
  if (!is.null(token$error)) {
    token$error
  } else {
    token$result
  }
}