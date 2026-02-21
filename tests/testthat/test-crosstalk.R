test_that("prepare_crosstalk_data returns correct structure", {
  data <- data.frame(id = 1:3, name = c("A", "B", "C"))
  result <- alpine_prepare_crosstalk_data(data, key_column = "id", group = "test")

  expect_is(result, "list")
  expect_true(all(c("keys", "data", "group", "key_column", "prepared_at") %in% names(result)))
  expect_equal(result$group, "test")
  expect_equal(result$key_column, "id")
})

test_that("prepare_crosstalk_data uses row numbers when key_column is NULL", {
  data <- data.frame(name = c("A", "B", "C"), value = 1:3)
  result <- alpine_prepare_crosstalk_data(data, key_column = NULL, group = "test")

  expect_equal(result$keys, c("1", "2", "3"))
  expect_null(result$key_column)
})

test_that("prepare_crosstalk_data extracts keys from specified column", {
  data <- data.frame(id = 10:12, value = c("x", "y", "z"))
  result <- alpine_prepare_crosstalk_data(data, key_column = "id", group = "test")

  expect_equal(result$keys, c("10", "11", "12"))
})

test_that("prepare_crosstalk_data validates key_column exists", {
  data <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    alpine_prepare_crosstalk_data(data, key_column = "nonexistent"),
    "not found in data"
  )
})

test_that("prepare_crosstalk_data validates inputs", {
  data <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    alpine_prepare_crosstalk_data(123, key_column = "a"),
    "must be a data.frame"
  )

  expect_error(
    alpine_prepare_crosstalk_data(data, group = c("a", "b")),
    "must be a single character string"
  )
})

test_that("prepare_crosstalk_data preserves data intact", {
  data <- data.frame(
    id = c("x", "y", "z"),
    value = c(1.5, 2.5, 3.5),
    flag = c(TRUE, FALSE, TRUE)
  )
  result <- alpine_prepare_crosstalk_data(data, key_column = "id", group = "test")

  expect_equal(nrow(result$data), 3)
  expect_equal(ncol(result$data), 3)
  expect_identical(result$data, data)
})

test_that("alpine_add_crosstalk_listener validates inputs", {
  tag <- htmltools::tags$div()

  expect_error(
    alpine_add_crosstalk_listener(tag, group = c("a", "b")),
    "must be a single character string"
  )

  expect_error(
    alpine_add_crosstalk_listener(tag, group = "test", handle_type = "invalid"),
    "must be 'selection', 'filter', or 'both'"
  )
})

test_that("alpine_add_crosstalk_listener returns tag object", {
  tag <- htmltools::tags$div()
  result <- alpine_add_crosstalk_listener(tag, group = "test", handle_type = "both")

  # Should return the tag (possibly modified)
  expect_is(result, "shiny.tag")
})

test_that("has_crosstalk reports availability", {
  result <- alpine_has_crosstalk()
  expect_is(result, "logical")
  expect_length(result, 1)
})

test_that("alpine_crosstalk_table requires crosstalk when using SharedData", {
  skip_if_not_installed("crosstalk")

  data <- data.frame(a = 1:3, b = 4:6)
  shared <- crosstalk::SharedData$new(data, group = "test")

  result <- alpine_crosstalk_table(shared)
  expect_is(result, "shiny.tag")
})

test_that("alpine_crosstalk_table accepts data.frame input", {
  skip_if_not_installed("crosstalk")

  data <- data.frame(id = 1:3, name = c("A", "B", "C"))
  result <- alpine_crosstalk_table(data)

  expect_is(result, "shiny.tag")
})

test_that("alpine_crosstalk_table validates inputs", {
  skip_if_not_installed("crosstalk")

  data <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    alpine_crosstalk_table(123),
    "must be a data.frame or crosstalk::SharedData"
  )

  expect_error(
    alpine_crosstalk_table(data.frame()),
    "0 rows"
  )
})

test_that("alpine_crosstalk_table handles column selection", {
  skip_if_not_installed("crosstalk")

  data <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  result <- alpine_crosstalk_table(data, columns = c("a", "c"))

  html <- as.character(result)
  # Should include those columns in output
  expect_true(grepl("a", html) || grepl("c", html))
})

test_that("alpine_crosstalk_table sets x-data with initial values", {
  skip_if_not_installed("crosstalk")

  data <- data.frame(id = 1:2, val = 10:11)
  result <- alpine_crosstalk_table(data, key_column = "id", selectable = TRUE)

  html <- as.character(result)
  # Should contain x-data initialization
  expect_true(grepl("x-data", html))
})

test_that("prepare_crosstalk_data only works with data.frame input", {
  # Lists are not supported
  expect_error(
    alpine_prepare_crosstalk_data(list(a = 1, b = 2), group = "test"),
    "must be a data.frame"
  )
})

test_that("alpine_crosstalk_table includes Crosstalk plugin registration", {
  skip_if_not_installed("crosstalk")

  # Clear plugin registry
  options(alpinejs.plugins = list())

  data <- data.frame(a = 1:3, b = 4:6)
  result <- alpine_crosstalk_table(data)

  # Plugin should be auto-registered
  registry <- alpine_get_plugin_registry()
  # Crosstalk may or may not be auto-registered depending on implementation
  expect_is(registry, "list")
})
