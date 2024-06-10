test_that("API key is set correctly", {
  set_api_key("dummy_key")
  expect_equal(get_api_key(), "dummy_key")
})

test_that("get_current_weather returns data", {
  set_api_key("your_real_api_key")
  result <- get_current_weather("London")
  expect_true("weather" %in% names(result))
})

test_that("get_weather_forecast returns data", {
  set_api_key("your_real_api_key")
  result <- get_weather_forecast("London")
  expect_true("list" %in% class(result))
})

