test_that("alpine_accordion creates valid structure", {
  accordion <- alpine_accordion(
    items = list(
      list(title = "Item 1", content = "Content 1"),
      list(title = "Item 2", content = "Content 2")
    )
  )
  
  html <- as.character(accordion)
  
  # Should contain x-data with collapsed state
  expect_true(grepl('x-data=', html))
  expect_true(grepl('collapsed', html))
  
  # Should contain titles
  expect_true(grepl('Item 1', html))
  expect_true(grepl('Item 2', html))
  
  # Should contain content
  expect_true(grepl('Content 1', html))
  expect_true(grepl('Content 2', html))
})

test_that("alpine_accordion validates items input", {
  expect_error(alpine_accordion(items = list()))
  expect_error(alpine_accordion(items = "not a list"))
})

test_that("alpine_accordion attaches Alpine dependency", {
  accordion <- alpine_accordion(
    items = list(list(title = "T", content = "C"))
  )
  
  expect_true(is_alpine_attached(accordion))
})

test_that("alpine_accordion uses x-show for visibility", {
  accordion <- alpine_accordion(
    items = list(
      list(title = "Item 1", content = "Content 1"),
      list(title = "Item 2", content = "Content 2")
    )
  )
  
  html <- as.character(accordion)
  
  # Should have x-show directives for each item
  expect_true(grepl('x-show=', html))
  expect_true(grepl('collapsed\\[0\\]', html))
  expect_true(grepl('collapsed\\[1\\]', html))
})

# Modal tests
test_that("alpine_modal creates valid structure", {
  modal <- alpine_modal(
    trigger_label = "Open",
    title = "My Modal",
    content = "Modal content"
  )
  
  html <- as.character(modal)
  
  # Should contain trigger button
  expect_true(grepl('Open', html))
  
  # Should contain title
  expect_true(grepl('My Modal', html))
  
  # Should contain content
  expect_true(grepl('Modal content', html))
  
  # Should have x-data with modalOpen state
  expect_true(grepl('x-data=', html))
  expect_true(grepl('modalOpen', html))
})

test_that("alpine_modal without trigger_label works", {
  modal <- alpine_modal(
    trigger_label = NULL,
    title = "Title",
    content = "Content"
  )
  
  expect_true(is_alpine_attached(modal))
})

test_that("alpine_modal attaches Alpine dependency", {
  modal <- alpine_modal(
    trigger_label = "Click",
    title = "Title",
    content = "Content"
  )
  
  expect_true(is_alpine_attached(modal))
})

test_that("alpine_modal uses x-show for visibility", {
  modal <- alpine_modal(
    trigger_label = "Show",
    title = "Modal",
    content = "Content"
  )
  
  html <- as.character(modal)
  expect_true(grepl('x-show=.*modalOpen', html))
})

test_that("alpine_modal has close button", {
  modal <- alpine_modal(
    trigger_label = "Open",
    title = "Title",
    content = "Content",
    close_label = "Dismiss"
  )
  
  html <- as.character(modal)
  expect_true(grepl('Dismiss', html))
})

# Tabs tests
test_that("alpine_tabs creates valid structure", {
  tabs <- alpine_tabs(
    tabs = list(
      list(label = "Tab 1", content = "Content 1"),
      list(label = "Tab 2", content = "Content 2"),
      list(label = "Tab 3", content = "Content 3")
    )
  )
  
  html <- as.character(tabs)
  
  # Should contain tab labels
  expect_true(grepl('Tab 1', html))
  expect_true(grepl('Tab 2', html))
  expect_true(grepl('Tab 3', html))
  
  # Should contain content
  expect_true(grepl('Content 1', html))
  expect_true(grepl('Content 2', html))
  expect_true(grepl('Content 3', html))
  
  # Should have x-data with activeTab state
  expect_true(grepl('x-data=', html))
  expect_true(grepl('activeTab', html))
})

test_that("alpine_tabs validates tabs input", {
  expect_error(alpine_tabs(tabs = list()))
  expect_error(alpine_tabs(tabs = "not a list"))
})

test_that("alpine_tabs attaches Alpine dependency", {
  tabs <- alpine_tabs(
    tabs = list(list(label = "L", content = "C"))
  )
  
  expect_true(is_alpine_attached(tabs))
})

test_that("alpine_tabs uses x-show for visibility", {
  tabs <- alpine_tabs(
    tabs = list(
      list(label = "Tab 1", content = "Content 1"),
      list(label = "Tab 2", content = "Content 2")
    )
  )
  
  html <- as.character(tabs)
  
  # Should have x-show directives for each content panel
  expect_true(grepl('x-show=', html))
  expect_true(grepl('activeTab === 0', html))
  expect_true(grepl('activeTab === 1', html))
})

test_that("alpine_tabs uses x-bind for button classes", {
  tabs <- alpine_tabs(
    tabs = list(
      list(label = "Tab 1", content = "Content 1")
    )
  )
  
  html <- as.character(tabs)
  
  # Tab buttons should have dynamic class binding
  expect_true(grepl('x-bind:class=', html))
})
