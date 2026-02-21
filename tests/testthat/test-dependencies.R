test_that("alpine_config returns htmlDependency object", {
  dep <- alpine_config()
  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "alpine")
})

test_that("CSP mode returns correct CDN URL", {
  dep <- alpine_config(csp = TRUE, version = "3.15.8")
  expect_true(grepl("@alpinejs/csp", attr(dep, "cdn_url")))
  expect_equal(dep$version, "3.15.8")
})

test_that("Non-CSP mode returns correct CDN URL", {
  dep <- alpine_config(csp = FALSE, version = "3.14.0")
  expect_true(grepl("alpinejs@3.14.0", attr(dep, "cdn_url")))
  expect_false(grepl("@alpinejs/csp", attr(dep, "cdn_url")))
})

test_that("Default CSP option is FALSE", {
  expect_false(getOption("alpinejs.use_csp", TRUE))
})

test_that("alpine_config validates version parameter", {
  expect_error(alpine_config(version = c("3.1", "3.2")))
  expect_error(alpine_config(version = 123))
})

test_that("is_alpine_attached detects attached dependencies", {
  tag_without_dep <- htmltools::div()
  expect_false(is_alpine_attached(tag_without_dep))
  
  tag_with_dep <- ensure_alpine_dependency(tag_without_dep)
  expect_true(is_alpine_attached(tag_with_dep))
})
