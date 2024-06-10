#' Set OpenWeather API Key
#'
#' Set your OpenWeather API key as an environment variable.
#' @param api_key Your OpenWeather API key.
#' @export
set_api_key <- function(api_key) {
  Sys.setenv(OPENWEATHER_API_KEY = api_key)
}

#' Get OpenWeather API Key
#'
#' Retrieve your OpenWeather API key from the environment variable.
#' @return The OpenWeather API key.
#' @export
get_api_key <- function() {
  key <- Sys.getenv("OPENWEATHER_API_KEY")
  if (key == "") stop("API key is not set. Use set_api_key() to set it.")
  return(key)
}
