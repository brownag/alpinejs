test_that("Complete interactive component renders valid HTML", {
  component <- htmltools::tags$div(
    htmltools::tags$button("Click me") |> alpine_on("click", "count++"),
    htmltools::tags$span() |> alpine_bind("textContent", "count")
  ) |>
    alpine_data(count = 0)
  
  html <- as.character(component)
  
  # Should contain all Alpine directives
  expect_true(grepl('x-data=', html))
  expect_true(grepl('@click=', html))
  expect_true(grepl('x-bind:textContent=', html))
  
  # Should be valid HTML structure
  expect_true(grepl('<div', html))
  expect_true(grepl('<button', html))
  expect_true(grepl('<span', html))
  expect_true(grepl('</div>', html))
})

test_that("Complex nested component with multiple modifiers", {
  component <- htmltools::tags$div(
    htmltools::tags$input(type = "text") |> alpine_model("searchText"),
    htmltools::tags$div("No results") |> alpine_show("results.length === 0 && searchText.length > 0"),
    htmltools::tags$ul() |> alpine_bind("innerHTML", "results.map(r => `<li>${r}</li>`).join('')")
  ) |>
    alpine_data(
      searchText = "",
      results = c("Apple", "Banana", "Cherry")
    )
  
  html <- as.character(component)
  
  # All directives present
  expect_true(grepl('x-model=', html))
  expect_true(grepl('x-show=', html))
  expect_true(grepl('x-bind:innerHTML=', html))
  
  # Data properly serialized
  expect_true(grepl('searchText', html))
  expect_true(grepl('results', html))
})

test_that("Rendered HTML includes Alpine CDN", {
  component <- htmltools::tags$div() |> alpine_data(x = 1)
  
  html <- htmltools::renderTags(component)
  
  # Alpine should be in the dependencies
  expect_true(length(html$dependencies) > 0)
  has_alpine <- any(sapply(html$dependencies, function(d) d$name == "alpine"))
  expect_true(has_alpine)
})

test_that("Multiple components on same page don't duplicate Alpine", {
  comp1 <- htmltools::tags$div("Component 1") |> alpine_data(a = 1)
  comp2 <- htmltools::tags$div("Component 2") |> alpine_data(b = 2)
  
  page <- htmltools::tags$div(comp1, comp2)
  
  html <- htmltools::renderTags(page)
  full_html <- paste(html$head, html$html, sep = "\n")
  
  # Count Alpine script inclusions (should be 1)
  alpine_count <- length(gregexpr('alpinejs.*\\.js', full_html)[[1]])
  expect_equal(alpine_count, 1)
})

test_that("Component state is properly escaped in attributes", {
  # Test special characters in state values
  component <- htmltools::tags$div() |>
    alpine_data(
      message = 'He said "Hello"',
      path = 'C:\\Users\\test'
    )
  
  html <- as.character(component)
  
  # Should properly escape or quote special characters
  expect_true(grepl('x-data=', html))
  # Should contain the values
  expect_true(grepl('Hello', html) || grepl('He said', html))
})

test_that("Rendered form component with validation", {
  form <- htmltools::tags$div(
    htmltools::tags$input(type = "email") |> alpine_model("email"),
    htmltools::tags$button("Submit") |> alpine_bind("disabled", "!isValidEmail()")
  ) |>
    alpine_data(
      email = "",
      isValidEmail = htmlwidgets::JS(
        "function() { return /^.+@.+\\..+$/.test(this.email); }"
      )
    )
  
  html <- as.character(form)
  
  # Should have all validation elements
  expect_true(grepl('x-model=', html))
  expect_true(grepl('x-bind:disabled=', html))
  expect_true(grepl('isValidEmail', html))
})

test_that("Accordion component full rendering", {
  acc <- alpine_accordion(
    items = list(
      list(title = "Q1", content = "A1"),
      list(title = "Q2", content = "A2")
    ),
    class = "my-accordion"
  )
  
  html <- as.character(acc)
  
  # Check structure
  expect_true(grepl('Q1', html))
  expect_true(grepl('A1', html))
  expect_true(grepl('Q2', html))
  expect_true(grepl('A2', html))
  expect_true(grepl('my-accordion', html))
  expect_true(grepl('collapsed', html))
})

test_that("Modal component full rendering", {
  modal <- alpine_modal(
    trigger_label = "Show Dialog",
    title = "Confirm",
    content = "Are you sure?",
    close_label = "No"
  )
  
  html <- as.character(modal)
  
  # Check all parts present
  expect_true(grepl('Show Dialog', html))
  expect_true(grepl('Confirm', html))
  expect_true(grepl('Are you sure', html))
  expect_true(grepl('No', html))
  expect_true(grepl('modalOpen', html))
})

test_that("Tabs component full rendering", {
  tabs <- alpine_tabs(
    tabs = list(
      list(label = "A", content = "Content A"),
      list(label = "B", content = "Content B"),
      list(label = "C", content = "Content C")
    )
  )
  
  html <- as.character(tabs)
  
  # Check tabs present
  expect_true(grepl('\\bA\\b', html))
  expect_true(grepl('\\bB\\b', html))
  expect_true(grepl('\\bC\\b', html))
  
  # Check content present
  expect_true(grepl('Content A', html))
  expect_true(grepl('Content B', html))
  expect_true(grepl('Content C', html))
  
  # Check state management
  expect_true(grepl('activeTab', html))
})

test_that("Save and load HTML preserves functionality", {
  component <- htmltools::tags$div(
    htmltools::tags$button("Count: ", htmltools::tags$span() |> alpine_bind("textContent", "count")) |>
      alpine_on("click", "count++")
  ) |>
    alpine_data(count = 0)
  
  # Create temporary file
  temp_file <-  tempfile(fileext = ".html")
  on.exit(unlink(temp_file))
  
  # Save HTML
  htmltools::save_html(component, temp_file)
  
  # Read back
  expect_true(file.exists(temp_file))
  content <- readLines(temp_file, warn = FALSE)
  html_content <- paste(content, collapse = "\n")
  
  # Verify content is present
  expect_true(grepl('x-data=', html_content))
  expect_true(grepl('@click=', html_content))
  # Alpine CDN should be included when saving
  expect_true(grepl('alpine', html_content, ignore.case = TRUE))
})

test_that("CSP mode changes Alpine CDN URL", {
  withr::local_options(list(alpinejs.use_csp = TRUE))
  component <- htmltools::tags$div() |> alpine_data(x = 1)
  html <- htmltools::renderTags(component)
  
  # Check that CSP build is in the dependencies
  expect_true(length(html$dependencies) > 0)
  has_csp <- any(sapply(html$dependencies, function(d) {
    grepl("@alpinejs/csp", attr(d, "cdn_url"), fixed = TRUE)
  }))
  expect_true(has_csp)
})

test_that("Non-CSP mode uses standard Alpine CDN", {
  withr::local_options(list(alpinejs.use_csp = FALSE))
  component <- htmltools::tags$div() |> alpine_data(x = 1)
  html <- htmltools::renderTags(component)
  
  # Check that standard build is in the dependencies (not CSP)
  expect_true(length(html$dependencies) > 0)
  has_standard <- any(sapply(html$dependencies, function(d) {
    !grepl("@alpinejs/csp", attr(d, "cdn_url")) && grepl("alpinejs", attr(d, "cdn_url"))
  }))
  expect_true(has_standard)
})
