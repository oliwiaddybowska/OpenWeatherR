library(httr)
library(jsonlite)

base_url <- "http://api.openweathermap.org/data/2.5"

#' Get Current Weather
#'
#' Retrieves current weather data for a given city.
#' @param city The name of the city.
#' @param units Units of measurement (default is "metric").
#' @return A list containing current weather data.
#' @export
get_current_weather <- function(city, units = "metric") {
  api_key <- get_api_key()
  url <- paste0(base_url, "/weather?q=", city, "&units=", units, "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  content(response, "parsed", simplifyVector = TRUE)
}

#' Get Weather Forecast
#'
#' Retrieves weather forecast data for a given city.
#' @param city The name of the city.
#' @param units Units of measurement (default is "metric").
#' @return A list containing weather forecast data.
#' @export
get_weather_forecast <- function(city, units = "metric") {
  api_key <- get_api_key()
  url <- paste0(base_url, "/forecast?q=", city, "&units=", units, "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  content(response, "parsed", simplifyVector = TRUE)
}

#' Get Historical Weather Data
#'
#' Retrieves historical weather data for a given city.
#' @param city The name of the city.
#' @param start_date The start date for the historical data (format: "YYYY-MM-DD").
#' @param end_date The end date for the historical data (format: "YYYY-MM-DD").
#' @param units Units of measurement (default is "metric").
#' @return A list containing historical weather data.
#' @export
get_historical_weather <- function(city, start_date, end_date, units = "metric") {
  api_key <- get_api_key()
  url <- paste0(base_url, "/onecall/timemachine?lat=", city_lat, "&lon=", city_lon,
                "&dt=", as.numeric(as.POSIXct(start_date)), "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  content(response, "parsed", simplifyVector = TRUE)
}

#' Get Air Pollution Data
#'
#' Retrieves current air pollution data for a given city.
#' @param city The name of the city.
#' @return A list containing air pollution data.
#' @export
get_air_pollution <- function(city) {
  api_key <- get_api_key()
  location <- get_location(city)
  url <- paste0(base_url, "/air_pollution?lat=", location$lat, "&lon=", location$lon, "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  content(response, "parsed", simplifyVector = TRUE)
}

#' Get Weather Data for Multiple Cities
#'
#' Retrieves current weather data for multiple cities.
#' @param cities A vector of city names.
#' @param units Units of measurement (default is "metric").
#' @return A list containing weather data for multiple cities.
#' @export
get_multiple_cities_weather <- function(cities, units = "metric") {
  api_key <- get_api_key()
  results <- list()
  for (city in cities) {
    url <- paste0(base_url, "/weather?q=", city, "&units=", units, "&appid=", api_key)
    response <- GET(url)
    stop_for_status(response)
    results[[city]] <- content(response, "parsed", simplifyVector = TRUE)
  }
  return(results)
}

#' Get City Coordinates
#'
#' Retrieves the latitude and longitude of a given city.
#' @param city The name of the city.
#' @return A list containing the latitude and longitude of the city.
#' @export
get_location <- function(city) {
  api_key <- get_api_key()
  url <- paste0("http://api.openweathermap.org/geo/1.0/direct?q=", city, "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  location <- content(response, "parsed", simplifyVector = TRUE)[[1]]
  return(list(lat = location$lat, lon = location$lon))
}

#' Get Daily Weather Forecast
#'
#' Retrieves daily weather forecast data for a given city.
#' @param city The name of the city.
#' @param units Units of measurement (default is "metric").
#' @return A list containing daily weather forecast data.
#' @export
get_daily_forecast <- function(city, units = "metric") {
  api_key <- get_api_key()
  location <- get_location(city)
  url <- paste0(base_url, "/onecall?lat=", location$lat, "&lon=", location$lon, "&exclude=hourly,minutely&units=", units, "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  content(response, "parsed", simplifyVector = TRUE)
}

#' Get Hourly Weather Forecast
#'
#' Retrieves hourly weather forecast data for a given city.
#' @param city The name of the city.
#' @param units Units of measurement (default is "metric").
#' @return A list containing hourly weather forecast data.
#' @export
get_hourly_forecast <- function(city, units = "metric") {
  api_key <- get_api_key()
  location <- get_location(city)
  url <- paste0(base_url, "/onecall?lat=", location$lat, "&lon=", location$lon, "&exclude=daily,minutely&units=", units, "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  content(response, "parsed", simplifyVector = TRUE)
}

#' Get Current Weather by Coordinates
#'
#' Retrieves current weather data for a given set of coordinates.
#' @param lat The latitude.
#' @param lon The longitude.
#' @param units Units of measurement (default is "metric").
#' @return A list containing current weather data.
#' @export
get_weather_by_coordinates <- function(lat, lon, units = "metric") {
  api_key <- get_api_key()
  url <- paste0(base_url, "/weather?lat=", lat, "&lon=", lon, "&units=", units, "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  content(response, "parsed", simplifyVector = TRUE)
}
