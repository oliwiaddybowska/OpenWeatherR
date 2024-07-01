library(httr)
library(jsonlite)

base_url <- "http://api.openweathermap.org/data/2.5"

#' Get Current Weather
#'
#' Retrieves current weather data for a given city, showing only the most important information.
#' @param city The name of the city.
#' @param units Units of measurement (default is "metric").
#' @return A list containing important current weather data: temperature, humidity, weather description, and wind speed.
#' @export
get_current_weather <- function(city, units = "metric") {
  api_key <- get_api_key()
  url <- paste0("http://api.openweathermap.org/data/2.5/weather?q=", city, "&units=", units, "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  weather_data <- content(response, "parsed")

  important_info <- list(
    temperature = weather_data$main$temp,
    humidity = weather_data$main$humidity,
    weather_description = weather_data$weather[[1]]$description,
    wind_speed = weather_data$wind$speed
  )

  return(important_info)
}
# Example usage
get_current_weather("London")

#' Get Weather Forecast
#'
#' Retrieves weather forecast data for a given city, showing only the most important information.
#' @param city The name of the city.
#' @param units Units of measurement (default is "metric").
#' @param forecast_time A string specifying the forecast time (e.g., "2024-07-03 15:00:00") or a day (e.g., "2024-07-03"). Default is NULL to return the next available forecast.
#' @return A list containing important weather forecast data: temperature, humidity, weather description, and wind speed.
#' @export
get_weather_forecast <- function(city, units = "metric", forecast_time = NULL) {
  api_key <- get_api_key()
  url <- paste0("http://api.openweathermap.org/data/2.5/forecast?q=", city, "&units=", units, "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  forecast_data <- content(response, "parsed")

  extract_important_info <- function(entry) {
    list(
      datetime = entry$dt_txt,
      temperature = entry$main$temp,
      humidity = entry$main$humidity,
      weather_description = entry$weather[[1]]$description,
      wind_speed = entry$wind$speed
    )
  }

  if (is.null(forecast_time)) {
    important_info <- extract_important_info(forecast_data$list[[1]])
  } else {

    matching_forecasts <- lapply(forecast_data$list, function(entry) {
      if (grepl(forecast_time, entry$dt_txt)) {
        return(extract_important_info(entry))
      } else {
        return(NULL)
      }
    })

    matching_forecasts <- Filter(Negate(is.null), matching_forecasts)

    if (length(matching_forecasts) == 0) {
      stop("No matching forecast data found for the specified time.")
    }

    important_info <- matching_forecasts[[1]]
  }

  return(important_info)
}
# Example usage
get_weather_forecast("London", forecast_time = "2024-07-03 15:00:00")

#' Get City Coordinates
#'
#' Retrieves the latitude and longitude of a given city.
#' @param city The name of the city.
#' @param state Optional. The state code.
#' @param country Optional. The country code.
#' @param limit Optional. The maximum number of results to return (default is 1).
#' @return A list containing the latitude and longitude of the city.
#' @export
get_location <- function(city, state = NULL, country = NULL, limit = 1) {
  api_key <- get_api_key()
  query <- paste0(city, if (!is.null(state)) paste0(",", state) else "", if (!is.null(country)) paste0(",", country) else "")
  url <- paste0("http://api.openweathermap.org/geo/1.0/direct?q=", query, "&limit=", limit, "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  location <- content(response, "parsed", simplifyVector = TRUE)

  if (length(location) == 0) {
    stop("No location data found for the specified city.")
  }

  lat <- location$lat
  lon <- location$lon

  if (is.null(lat) || is.null(lon)) {
    stop("Latitude and longitude data not found for the specified city.")
  }

  return(list(lat = lat, lon = lon))
}
# Example usage
get_location("London")

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
# Example usage
get_air_pollution("London")


#' Get Current Weather by Coordinates
#'
#' Retrieves current weather data for a given set of coordinates, showing only the most important information.
#' @param lat The latitude.
#' @param lon The longitude.
#' @param units Units of measurement (default is "metric").
#' @return A list containing important current weather data: temperature, humidity, weather description, and wind speed.
#' @export
get_weather_by_coordinates <- function(lat, lon, units = "metric") {
  api_key <- get_api_key()
  url <- paste0("http://api.openweathermap.org/data/2.5/weather?lat=", lat, "&lon=", lon, "&units=", units, "&appid=", api_key)
  response <- GET(url)
  stop_for_status(response)
  weather_data <- content(response, "parsed")

  important_info <- list(
    temperature = weather_data$main$temp,
    humidity = weather_data$main$humidity,
    weather_description = weather_data$weather[[1]]$description,
    wind_speed = weather_data$wind$speed
  )

  return(important_info)
}
# Example usage
get_weather_by_coordinates(51.5073, -0.1276)






