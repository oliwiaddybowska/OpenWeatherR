test_that("API key is set correctly", {
  set_api_key("dummy_key")
  expect_equal(get_api_key(), "dummy_key")
})

test_that("get_api_key ruturns correct key", {
  get_api_key()
  expect_equal("a12024b0d13d0418b019418787fd4a7d")
})

test_that("get_current_weather returns data", {
  set_api_key("a12024b0d13d0418b019418787fd4a7d")
  result <- get_current_weather("London")
  expect_true("weather" %in% names(result))
})

test_that("get_weather_forecast returns data", {
  set_api_key("a12024b0d13d0418b019418787fd4a7d")
  result <- get_weather_forecast("London")
  expect_true("list" %in% class(result))
})

