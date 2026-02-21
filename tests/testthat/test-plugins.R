test_that("register_alpine_plugin adds plugin to registry", {
  # Start with clean registry
  options(alpinejs.plugins = list())

  alpine_register_plugin("test-plugin", version = "1.0.0")
  registry <- alpine_get_plugin_registry()

  expect_true("test-plugin" %in% names(registry))
  expect_equal(registry$`test-plugin`$name, "test-plugin")
  expect_equal(registry$`test-plugin`$version, "1.0.0")
})

test_that("register_alpine_plugin sets default version", {
  options(alpinejs.plugins = list())
  options(alpinejs.version = "3.15.8")

  # Register without version - should use default
  alpine_register_plugin("auto-version")
  registry <- alpine_get_plugin_registry()

  expect_equal(registry$`auto-version`$version, "3.15.8")
})

test_that("register_alpine_plugin validates name parameter", {
  options(alpinejs.plugins = list())

  expect_error(
    alpine_register_plugin(123),
    "name must be a non-empty character string"
  )

  expect_error(
    alpine_register_plugin(""),
    "name must be a non-empty character string"
  )

  expect_error(
    alpine_register_plugin(c("plugin1", "plugin2")),
    "name must be a non-empty character string"
  )
})

test_that("register_alpine_plugin validates version parameter", {
  options(alpinejs.plugins = list())

  expect_error(
    alpine_register_plugin("test", version = 1.0),
    "version must be a character string"
  )
})

test_that("register_alpine_plugin prevents duplicate registration", {
  options(alpinejs.plugins = list())

  alpine_register_plugin("persist", version = "1.0.0")
  # Silently skip re-registration (no warning)
  alpine_register_plugin("persist", version = "2.0.0")

  # Original version should still be in registry
  registry <- alpine_get_plugin_registry()
  expect_equal(registry$persist$version, "1.0.0")
})

test_that("register_alpine_plugin warns on invalid version format", {
  options(alpinejs.plugins = list())

  expect_warning(
    alpine_register_plugin("test", version = "invalid"),
    "doesn't match expected SemVer format"
  )
})

test_that("get_plugin_registry returns empty list initially", {
  options(alpinejs.plugins = list())
  registry <- alpine_get_plugin_registry()

  expect_is(registry, "list")
  expect_length(registry, 0)
})

test_that("get_plugin_registry returns all registered plugins", {
  options(alpinejs.plugins = list())

  alpine_register_plugin("plugin1", version = "1.0.0")
  alpine_register_plugin("plugin2", version = "2.0.0")
  alpine_register_plugin("plugin3", version = "3.0.0")

  registry <- alpine_get_plugin_registry()

  expect_length(registry, 3)
  expect_true(all(c("plugin1", "plugin2", "plugin3") %in% names(registry)))
})

test_that("is_plugin_registered returns TRUE for registered plugins", {
  options(alpinejs.plugins = list())

  alpine_register_plugin("persist", version = "1.0.0")

  expect_true(alpine_is_plugin_registered("persist"))
  expect_false(alpine_is_plugin_registered("nonexistent"))
})

test_that("get_plugin_cdn_url generates correct CDN URLs", {
  options(alpinejs.plugins = list())

  alpine_register_plugin("persist", version = "3.15.8", npm_scope = "alpinejs")
  url <- get_plugin_cdn_url("persist")

  expected <- "https://cdn.jsdelivr.net/npm/@alpinejs/persist@3.15.8/dist/persist.min.js"
  expect_equal(url, expected)
})

test_that("get_plugin_cdn_url returns NA for unregistered plugins", {
  options(alpinejs.plugins = list())

  url <- get_plugin_cdn_url("nonexistent")

  expect_true(is.na(url))
})

test_that("get_plugin_cdn_url respects custom npm_scope", {
  options(alpinejs.plugins = list())

  alpine_register_plugin(
    "custom",
    version = "1.0.0",
    npm_scope = "mycompany"
  )
  url <- get_plugin_cdn_url("custom")

  expected <- "https://cdn.jsdelivr.net/npm/@mycompany/custom@1.0.0/dist/custom.min.js"
  expect_equal(url, expected)
})

test_that("html_dependency_plugin creates valid dependency", {
  options(alpinejs.plugins = list())

  alpine_register_plugin("persist", version = "3.15.8")
  dep <- html_dependency_plugin("persist")

  expect_is(dep, "html_dependency")
  expect_equal(dep$name, "alpine-plugin-persist")
  expect_equal(dep$version, "3.15.8")
})

test_that("html_dependency_plugin returns NULL for unregistered plugins", {
  options(alpinejs.plugins = list())

  dep <- html_dependency_plugin("nonexistent")

  expect_null(dep)
})

test_that("ensure_plugin_dependency warns for unregistered plugins", {
  options(alpinejs.plugins = list())

  expect_warning(
    ensure_plugin_dependency("missing"),
    "not registered"
  )
})

test_that("setup_persist registers persist plugin", {
  options(alpinejs.plugins = list())

  alpine_setup_persist()
  registry <- alpine_get_plugin_registry()

  expect_true("persist" %in% names(registry))
  expect_equal(registry$persist$name, "persist")
})

test_that("setup_mask registers mask plugin", {
  options(alpinejs.plugins = list())

  alpine_setup_mask()
  registry <- alpine_get_plugin_registry()

  expect_true("mask" %in% names(registry))
  expect_equal(registry$mask$name, "mask")
})

test_that("setup_sort registers sort plugin", {
  options(alpinejs.plugins = list())

  alpine_setup_sort()
  registry <- alpine_get_plugin_registry()

  expect_true("sort" %in% names(registry))
  expect_equal(registry$sort$name, "sort")
})

test_that("multiple plugins can be registered simultaneously", {
  options(alpinejs.plugins = list())

  alpine_setup_persist()
  alpine_setup_mask()
  alpine_setup_sort()

  registry <- alpine_get_plugin_registry()

  expect_length(registry, 3)
  expect_true(all(c("persist", "mask", "sort") %in% names(registry)))
})

test_that("plugin metadata is correctly stored", {
  options(alpinejs.plugins = list())

  alpine_register_plugin(
    "example",
    version = "2.5.0",
    npm_scope = "test-scope",
    required_alpine_version = "3.5.0",
    init_code = "alert('test')"
  )

  registry <- alpine_get_plugin_registry()
  plugin <- registry$example

  expect_equal(plugin$name, "example")
  expect_equal(plugin$version, "2.5.0")
  expect_equal(plugin$npm_scope, "test-scope")
  expect_equal(plugin$required_alpine_version, "3.5.0")
  expect_equal(plugin$init_code, "alert('test')")
  expect_true(!is.null(plugin$registered_at))
})

test_that("register_alpine_plugin returns invisibly TRUE", {
  options(alpinejs.plugins = list())

  result <- alpine_register_plugin("test", version = "1.0.0")

  expect_true(result)
})

test_that("setup functions return invisibly TRUE", {
  options(alpinejs.plugins = list())

  result1 <- alpine_setup_persist()
  result2 <- alpine_setup_mask()
  result3 <- alpine_setup_sort()

  expect_true(result1)
  expect_true(result2)
  expect_true(result3)
})
