test_that("x_model generates correct attribute without modifiers", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "userName")
  html <- as.character(result)

  expect_true(grepl('x-model="userName"', html))
  expect_false(grepl('x-model\\.', html))
})

test_that("alpine_model with .lazy generates correct modifier syntax", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "userName", .lazy = TRUE)
  html <- as.character(result)

  expect_true(grepl('x-model\\.lazy="userName"', html))
})

test_that("alpine_model with .debounce generates correct modifier syntax", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "searchQuery", .debounce = 300)
  html <- as.character(result)

  expect_true(grepl('x-model\\.debounce\\.300="searchQuery"', html))
})

test_that("alpine_model with .number generates correct modifier syntax", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "userAge", .number = TRUE)
  html <- as.character(result)

  expect_true(grepl('x-model\\.number="userAge"', html))
})

test_that("alpine_model with .trim generates correct modifier syntax", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "userName", .trim = TRUE)
  html <- as.character(result)

  expect_true(grepl('x-model\\.trim="userName"', html))
})

test_that("alpine_model with .lazy and .debounce generates correct modifier order", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "searchQuery", .lazy = TRUE, .debounce = 500)
  html <- as.character(result)

  # Alpine.js order: .lazy -> .debounce.NNN -> .number -> .trim
  expect_true(grepl('x-model\\.lazy\\.debounce\\.500="searchQuery"', html))
})

test_that("x_model with all modifiers generates correct modifier order", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "count", .lazy = TRUE, .debounce = 250, .number = TRUE, .trim = TRUE)
  html <- as.character(result)

  # Order: lazy -> debounce.250 -> number -> trim
  expect_true(grepl('x-model\\.lazy\\.debounce\\.250\\.number\\.trim="count"', html))
})

test_that("alpine_model with .number and .trim (no lazy/debounce)", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "formValue", .number = TRUE, .trim = TRUE)
  html <- as.character(result)

  expect_true(grepl('x-model\\.number\\.trim="formValue"', html))
})

test_that("x_model validates .debounce as numeric", {
  tag <- htmltools::tags$input(type = "text")

  expect_error(
    alpine_model(tag, "search", .debounce = "500"),
    ".debounce must be NULL or a single numeric value"
  )
})

test_that("x_model validates .debounce as non-negative", {
  tag <- htmltools::tags$input(type = "text")

  expect_error(
    alpine_model(tag, "search", .debounce = -100),
    ".debounce must be >= 0"
  )
})

test_that("x_model warns if .debounce is non-integer", {
  tag <- htmltools::tags$input(type = "text")

  expect_warning(
    alpine_model(tag, "search", .debounce = 300.7),
    ".debounce should be an integer"
  )
})

test_that("x_model rounds .debounce to nearest integer", {
  tag <- htmltools::tags$input(type = "text")
  result <- suppressWarnings(alpine_model(tag, "search", .debounce = 300.7))
  html <- as.character(result)

  expect_true(grepl('x-model\\.debounce\\.301="search"', html))
})

test_that("x_model validates .lazy as logical", {
  tag <- htmltools::tags$input(type = "text")

  expect_error(
    alpine_model(tag, "search", .lazy = "yes"),
    ".lazy must be a single logical value"
  )
})

test_that("x_model validates .number as logical", {
  tag <- htmltools::tags$input(type = "text")

  expect_error(
    alpine_model(tag, "count", .number = 1),
    ".number must be a single logical value"
  )
})

test_that("x_model validates .trim as logical", {
  tag <- htmltools::tags$input(type = "text")

  expect_error(
    alpine_model(tag, "name", .trim = "yes"),
    ".trim must be a single logical value"
  )
})

test_that("x_model includes Alpine dependency", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "userName", .debounce = 300)

  # Check that Alpine dependency is included
  deps <- htmltools::findDependencies(result)
  dep_names <- sapply(deps, function(d) d[["name"]])
  expect_true("alpine" %in% dep_names)
})

test_that("x_model with .debounce = 0 works correctly", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "instant", .debounce = 0)
  html <- as.character(result)

  expect_true(grepl('x-model\\.debounce\\.0="instant"', html))
})

test_that("x_model with large .debounce value works", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "search", .debounce = 5000)
  html <- as.character(result)

  expect_true(grepl('x-model\\.debounce\\.5000="search"', html))
})

test_that("x_model modifiers work with nested variable paths", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "form.user.email", .lazy = TRUE, .number = FALSE)
  html <- as.character(result)

  expect_true(grepl('x-model\\.lazy="form\\.user\\.email"', html))
})

test_that("x_model with .debounce and .trim generates correct order", {
  tag <- htmltools::tags$input(type = "text")
  result <- alpine_model(tag, "text", .debounce = 400, .trim = TRUE)
  html <- as.character(result)

  # Order: debounce.400 -> trim (lazy is FALSE, number is FALSE)
  expect_true(grepl('x-model\\.debounce\\.400\\.trim="text"', html))
})

test_that("x_model validates variable as single character", {
  tag <- htmltools::tags$input(type = "text")

  expect_error(
    alpine_model(tag, c("var1", "var2")),
    "variable must be a single character string"
  )
})

test_that("x_model validates variable as character (not numeric)", {
  tag <- htmltools::tags$input(type = "text")

  expect_error(
    alpine_model(tag, 123),
    "variable must be a single character string"
  )
})

test_that("x_model produces pipe-compatible output", {
  result1 <- htmltools::tags$input(type = "text") |>
    alpine_model("search", .debounce = 300)

  result2 <- alpine_model(htmltools::tags$input(type = "text"), "search", .debounce = 300)

  # Both should produce identical HTML output
  html1 <- htmltools::renderTags(result1)$html
  html2 <- htmltools::renderTags(result2)$html

  expect_equal(html1, html2)
})

test_that("x_model with modifiers on different input types", {
  # Test textarea
  textarea <- htmltools::tags$textarea() |>
    alpine_model("content", .debounce = 200)
  html <- as.character(textarea)
  expect_true(grepl('x-model\\.debounce\\.200="content"', html))

  # Test checkbox
  checkbox <- htmltools::tags$input(type = "checkbox") |>
    alpine_model("agreed", .lazy = TRUE)
  html <- as.character(checkbox)
  expect_true(grepl('x-model\\.lazy="agreed"', html))

  # Test select
  select <- htmltools::tags$select(
    htmltools::tags$option("Option 1")
  ) |>
    alpine_model("choice", .lazy = TRUE)
  html <- as.character(select)
  expect_true(grepl('x-model\\.lazy="choice"', html))
})
