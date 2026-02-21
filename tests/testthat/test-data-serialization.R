test_that("serialize_r_to_json handles numeric values", {
  result <- serialize_r_to_json(count = 42, ratio = 3.14)
  
  expect_true(grepl('"count":42', result))
  expect_true(grepl('"ratio":3.14', result))
})

test_that("serialize_r_to_json handles logical values", {
  result <- serialize_r_to_json(active = TRUE, disabled = FALSE)
  
  expect_true(grepl('"active":true', result))
  expect_true(grepl('"disabled":false', result))
})

test_that("serialize_r_to_json handles character values", {
  result <- serialize_r_to_json(name = "Alice", message = "Hello World")
  
  expect_true(grepl('"name":"Alice"', result))
  expect_true(grepl('"message":"Hello World"', result))
})

test_that("serialize_r_to_json handles vectors", {
  result <- serialize_r_to_json(numbers = c(1, 2, 3), names = c("a", "b"))
  
  expect_true(grepl('"numbers":\\[1,2,3\\]', result))
  expect_true(grepl('"names":\\["a","b"\\]', result))
})

test_that("serialize_r_to_json handles lists as objects", {
  result <- serialize_r_to_json(config = list(version = "1.0", debug = TRUE))
  
  expect_true(grepl('"config":\\{', result))
  expect_true(grepl('"version":"1.0"', result))
  expect_true(grepl('"debug":true', result))
})

test_that("serialize_r_to_json converts factors to character", {
  result <- serialize_r_to_json(category = factor(c("A", "B", "C")))
  
  # Should not contain factor internals
  expect_false(grepl('levels', result))
  # Should be a character array
  expect_true(grepl('\\["A","B","C"\\]', result))
})

test_that("serialize_r_to_json converts dates to ISO 8601", {
  date <- as.Date("2026-02-21")
  result <- serialize_r_to_json(today = date)
  
  expect_true(grepl('"today":"2026-02-21"', result))
})

test_that("serialize_r_to_json converts POSIXct to ISO 8601", {
  time <- as.POSIXct("2026-02-21 12:30:45", tz = "UTC")
  result <- serialize_r_to_json(timestamp = time)
  
  expect_true(grepl('"timestamp":"2026-02-21T', result))
})

test_that("serialize_r_to_json converts NA to null", {
  result <- serialize_r_to_json(
    missing_numeric = NA,
    missing_char = NA_character_
  )
  
  expect_true(grepl('"missing_numeric":null', result))
  expect_true(grepl('"missing_char":null', result))
})

test_that("serialize_r_to_json handles data frames", {
  df <- data.frame(
    id = 1:2,
    name = c("Alice", "Bob"),
    stringsAsFactors = FALSE
  )
  
  result <- serialize_r_to_json(data = df)
  
  # Should be an array of objects
  expect_true(grepl('"data":\\[', result))
  expect_true(grepl('"id":1', result))
  expect_true(grepl('"name":"Alice"', result))
})

test_that("serialize_r_to_json preserves JS() expressions", {
  result <- serialize_r_to_json(
    count = 0,
    double = htmlwidgets::JS("this.count * 2")
  )
  
  expect_true(grepl('"count":0', result))
  expect_true(grepl('"double":this.count \\* 2', result))
})

test_that("serialize_r_to_json handles empty collections", {
  result <- serialize_r_to_json(
    empty_vec = c(),
    empty_list = list()
  )
  
  # Empty vectors and empty unnamed lists both serialize to arrays in JSON
  expect_true(grepl('"empty_vec":\\[\\]', result))
  expect_true(grepl('"empty_list":\\[\\]', result))
})

test_that("serialize_r_to_json handles nested structures", {
  result <- serialize_r_to_json(
    user = list(
      name = "Alice",
      preferences = list(
        theme = "dark",
        notifications = TRUE
      )
    )
  )
  
  expect_true(grepl('"user":\\{', result))
  expect_true(grepl('"name":"Alice"', result))
  expect_true(grepl('"preferences":\\{', result))
  expect_true(grepl('"theme":"dark"', result))
})

test_that("is_js_expression detects JS objects", {
  js_expr <- htmlwidgets::JS("function() { }")
  regular_value <- "not js"
  
  expect_true(is_js_expression(js_expr))
  expect_false(is_js_expression(regular_value))
})

test_that("convert_r_type_for_json preserves JS expressions", {
  js_expr <- htmlwidgets::JS("1 + 1")
  result <- convert_r_type_for_json(js_expr)
  
  expect_identical(result, js_expr)
})

test_that("convert_r_type_for_json converts factors", {
  factor_val <- factor(c("A", "B", "C"))
  result <- convert_r_type_for_json(factor_val)
  
  expect_true(is.character(result))
  expect_equal(result, c("A", "B", "C"))
})

test_that("convert_r_type_for_json handles NULL", {
  result <- convert_r_type_for_json(NULL)
  # NULL is converted to an empty list which serializes to []
  expect_true(is.list(result) && length(result) == 0)
})
