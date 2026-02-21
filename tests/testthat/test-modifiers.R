test_that("alpine_data attaches x-data attribute", {
  tag <- htmltools::tags$div() |>
    alpine_data(count = 0)
  
  html <- as.character(tag)
  expect_true(grepl('x-data=', html))
  # JSON is HTML-escaped in attributes (&quot; instead of ")
  expect_true(grepl('&quot;count&quot;:0', html) || grepl('"count":0', html))
})

test_that("alpine_data serializes multiple values", {
  tag <- htmltools::tags$div() |>
    alpine_data(a = 1, b = TRUE, c = "hello")
  
  html <- as.character(tag)
  # JSON is HTML-escaped in attributes
  expect_true(grepl('&quot;a&quot;:1', html) || grepl('"a":1', html))
  expect_true(grepl('&quot;b&quot;:true', html) || grepl('"b":true', html))
  expect_true(grepl('&quot;c&quot;:&quot;hello&quot;', html) || grepl('"c":"hello"', html))
})

test_that("alpine_data auto-injects Alpine dependency", {
  tag <- htmltools::tags$div() |>
    alpine_data(count = 0)
  
  expect_true(is_alpine_attached(tag))
})

test_that("alpine_data returns tag unchanged type", {
  tag <- htmltools::tags$div(class = "test")
  result <- tag |> alpine_data(x = 1)
  
  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "div")
})

# x-on tests
test_that("alpine_on attaches event attribute", {
  tag <- htmltools::tags$button("Click") |>
    alpine_on("click", "count++")
  
  html <- as.character(tag)
  expect_true(grepl('@click=', html))
  expect_true(grepl('count\\+\\+', html))
})

test_that("alpine_on auto-injects Alpine dependency", {
  tag <- htmltools::tags$button() |>
    alpine_on("click", "doSomething()")
  
  expect_true(is_alpine_attached(tag))
})

test_that("alpine_on supports various events", {
  events <- c("click", "keydown", "input", "change", "submit")
  
  for (event in events) {
    tag <- htmltools::tags$div() |>
      alpine_on(event, "handler()")
    
    html <- as.character(tag)
    expect_true(grepl(paste0("@", event), html))
  }
})

# x-show tests
test_that("alpine_show attaches x-show attribute", {
  tag <- htmltools::tags$div("content") |>
    alpine_show("isVisible")
  
  html <- as.character(tag)
  expect_true(grepl('x-show=', html))
  expect_true(grepl('isVisible', html))
})

test_that("alpine_show auto-injects Alpine dependency", {
  tag <- htmltools::tags$div() |>
    alpine_show("true")
  
  expect_true(is_alpine_attached(tag))
})

# x-bind tests
test_that("alpine_bind attaches x-bind:attr attribute", {
  tag <- htmltools::tags$button() |>
    alpine_bind("disabled", "!isValid")
  
  html <- as.character(tag)
  expect_true(grepl('x-bind:disabled=', html))
  expect_true(grepl('isValid', html))
})

test_that("alpine_bind auto-injects Alpine dependency", {
  tag <- htmltools::tags$div() |>
    alpine_bind("class", "activeClass")
  
  expect_true(is_alpine_attached(tag))
})

test_that("alpine_bind supports various attributes", {
  attrs <- c("class", "disabled", "hidden", "style", "href")
  
  for (attr in attrs) {
    tag <- htmltools::tags$div() |>
      alpine_bind(attr, "someValue")
    
    html <- as.character(tag)
    expect_true(grepl(paste0('x-bind:', attr), html))
  }
})

# x-model tests
test_that("alpine_model attaches x-model attribute", {
  tag <- htmltools::tags$input(type = "text") |>
    alpine_model("userName")
  
  html <- as.character(tag)
  expect_true(grepl('x-model=', html))
  expect_true(grepl('userName', html))
})

test_that("alpine_model auto-injects Alpine dependency", {
  tag <- htmltools::tags$input() |>
    alpine_model("anyValue")
  
  expect_true(is_alpine_attached(tag))
})

# Piping tests
test_that("Modifiers can be chained with pipe", {
  tag <- htmltools::tags$div() |>
    alpine_data(count = 0) |>
    alpine_on("click", "count++") |>
    alpine_show("count > 0")
  
  html <- as.character(tag)
  expect_true(grepl('x-data=', html))
  expect_true(grepl('@click=', html))
  expect_true(grepl('x-show=', html))
})

# Dependency deduplication tests
test_that("Multiple modifiers don't duplicate Alpine dependency", {
  tag <- htmltools::tags$div() |>
    alpine_data(x = 1) |>
    alpine_on("click", "x++") |>
    alpine_show("x > 0")
  
  deps <- htmltools::findDependencies(tag)
  alpine_deps <- Filter(function(d) d$name == "alpine", deps)
  
  expect_equal(length(alpine_deps), 1, info = "Should have exactly 1 Alpine dependency")
})
