# Tests for new Alpine.js directive functions (Phases 1-5)
# Covers: alpine_html, alpine_effect, alpine_ref, alpine_cloak, alpine_ignore,
#         alpine_if, alpine_modelable, alpine_transition, alpine_teleport, alpine_id,
#         alpine_watch, alpine_dispatch, alpine_next_tick, alpine_refs, alpine_store_js,
#         alpine_el, alpine_global_data, alpine_global_store

# ---- alpine_html ----

test_that("alpine_html requires opt-in", {
  withr::with_options(list(alpinejs.unsafe = NULL), {
    withr::with_envvar(list(R_ALPINEJS_UNSAFE = ""), {
      expect_error(
        htmltools::tags$div() |> alpine_html("content"),
        "XSS"
      )
    })
  })
})

test_that("alpine_html works with options opt-in", {
  withr::with_options(list(alpinejs.unsafe = TRUE), {
    tag <- htmltools::tags$div() |> alpine_html("content")
    html <- as.character(tag)
    expect_true(grepl("x-html=", html))
    expect_true(grepl("content", html))
  })
})

test_that("alpine_html works with env var opt-in", {
  withr::with_envvar(list(R_ALPINEJS_UNSAFE = "1"), {
    tag <- htmltools::tags$div() |> alpine_html("rawHtml")
    html <- as.character(tag)
    expect_true(grepl("x-html=", html))
  })
})

test_that("alpine_html injects Alpine dependency", {
  withr::with_options(list(alpinejs.unsafe = TRUE), {
    tag <- htmltools::tags$div() |> alpine_html("content")
    expect_true(is_alpine_attached(tag))
  })
})

# ---- alpine_effect ----

test_that("alpine_effect attaches x-effect attribute", {
  tag <- htmltools::tags$div() |>
    alpine_effect("console.log(count)")
  html <- as.character(tag)
  expect_true(grepl("x-effect=", html))
  expect_true(grepl("console.log", html))
})

test_that("alpine_effect injects Alpine dependency", {
  tag <- htmltools::tags$div() |> alpine_effect("doSomething()")
  expect_true(is_alpine_attached(tag))
})

test_that("alpine_effect validates expression", {
  expect_error(htmltools::tags$div() |> alpine_effect(123), "character")
})

# ---- alpine_ref ----

test_that("alpine_ref attaches x-ref attribute", {
  tag <- htmltools::tags$input() |> alpine_ref("myInput")
  html <- as.character(tag)
  expect_true(grepl("x-ref=", html))
  expect_true(grepl("myInput", html))
})

test_that("alpine_ref injects Alpine dependency", {
  tag <- htmltools::tags$input() |> alpine_ref("el")
  expect_true(is_alpine_attached(tag))
})

test_that("alpine_ref validates ref_name", {
  expect_error(htmltools::tags$input() |> alpine_ref(123), "character")
})

# ---- alpine_cloak ----

test_that("alpine_cloak attaches x-cloak attribute", {
  tag <- htmltools::tags$div("content") |> alpine_cloak()
  html <- as.character(tag)
  expect_true(grepl("x-cloak", html))
})

test_that("alpine_cloak injects CSS dependency", {
  tag <- htmltools::tags$div() |> alpine_cloak()
  deps <- htmltools::findDependencies(tag)
  dep_names <- vapply(deps, `[[`, character(1), "name")
  expect_true("alpine-cloak-css" %in% dep_names)
})

test_that("alpine_cloak injected CSS contains the required rule", {
  tag <- htmltools::tags$div() |> alpine_cloak()
  deps <- htmltools::findDependencies(tag)
  cloak_dep <- deps[vapply(deps, function(d) d$name == "alpine-cloak-css", logical(1))][[1]]
  expect_true(grepl("x-cloak", cloak_dep$head))
  expect_true(grepl("display: none", cloak_dep$head))
})

test_that("alpine_cloak injects Alpine dependency", {
  tag <- htmltools::tags$div() |> alpine_cloak()
  expect_true(is_alpine_attached(tag))
})

# ---- alpine_ignore ----

test_that("alpine_ignore attaches x-ignore attribute", {
  tag <- htmltools::tags$div("widget") |> alpine_ignore()
  html <- as.character(tag)
  expect_true(grepl("x-ignore", html))
})

test_that("alpine_ignore injects Alpine dependency", {
  tag <- htmltools::tags$div() |> alpine_ignore()
  expect_true(is_alpine_attached(tag))
})

# ---- alpine_if ----

test_that("alpine_if wraps content in <template>", {
  result <- alpine_if(htmltools::tags$div("content"), "open")
  html <- as.character(result)
  expect_true(grepl("<template", html))
  expect_true(grepl("x-if=", html))
  expect_true(grepl("open", html))
})

test_that("alpine_if injects Alpine dependency", {
  result <- alpine_if(htmltools::tags$div(), "isVisible")
  expect_true(is_alpine_attached(result))
})

test_that("alpine_if validates condition", {
  expect_error(
    alpine_if(htmltools::tags$div(), 123),
    "character"
  )
})

# ---- alpine_modelable ----

test_that("alpine_modelable attaches x-modelable attribute", {
  tag <- htmltools::tags$div() |> alpine_modelable("value")
  html <- as.character(tag)
  expect_true(grepl("x-modelable=", html))
  expect_true(grepl("value", html))
})

test_that("alpine_modelable injects Alpine dependency", {
  tag <- htmltools::tags$div() |> alpine_modelable("val")
  expect_true(is_alpine_attached(tag))
})

test_that("alpine_modelable validates property", {
  expect_error(htmltools::tags$div() |> alpine_modelable(1), "character")
})

# ---- alpine_transition ----

test_that("alpine_transition with no args adds plain x-transition", {
  tag <- htmltools::tags$div() |> alpine_transition()
  html <- as.character(tag)
  expect_true(grepl("x-transition", html))
})

test_that("alpine_transition .duration builds modifier string", {
  tag <- htmltools::tags$div() |> alpine_transition(.duration = 500)
  html <- as.character(tag)
  expect_true(grepl("x-transition.duration.500ms", html))
})

test_that("alpine_transition .opacity builds modifier string", {
  tag <- htmltools::tags$div() |> alpine_transition(.opacity = TRUE)
  html <- as.character(tag)
  expect_true(grepl("x-transition.opacity", html))
})

test_that("alpine_transition .scale builds modifier string", {
  tag <- htmltools::tags$div() |> alpine_transition(.scale = TRUE, .scale_value = 80)
  html <- as.character(tag)
  expect_true(grepl("x-transition.scale.80", html))
})

test_that("alpine_transition CSS class mode uses separate attributes", {
  tag <- htmltools::tags$div() |>
    alpine_transition(
      .enter       = "transition ease-out duration-300",
      .enter_start = "opacity-0"
    )
  html <- as.character(tag)
  expect_true(grepl("x-transition:enter", html))
  expect_true(grepl("x-transition:enter-start", html))
})

test_that("alpine_transition injects Alpine dependency", {
  tag <- htmltools::tags$div() |> alpine_transition()
  expect_true(is_alpine_attached(tag))
})

# ---- alpine_teleport ----

test_that("alpine_teleport wraps content in <template>", {
  result <- alpine_teleport(htmltools::tags$div("modal"), "body")
  html <- as.character(result)
  expect_true(grepl("<template", html))
  expect_true(grepl("x-teleport=", html))
  expect_true(grepl("body", html))
})

test_that("alpine_teleport injects Alpine dependency", {
  result <- alpine_teleport(htmltools::tags$div(), "#root")
  expect_true(is_alpine_attached(result))
})

test_that("alpine_teleport validates selector", {
  expect_error(alpine_teleport(htmltools::tags$div(), 123), "character")
})

# ---- alpine_id ----

test_that("alpine_id attaches x-id attribute with JS array", {
  tag <- htmltools::tags$div() |> alpine_id("tooltip-button", "tooltip-panel")
  html <- as.character(tag)
  expect_true(grepl("x-id=", html))
  expect_true(grepl("tooltip-button", html))
  expect_true(grepl("tooltip-panel", html))
})

test_that("alpine_id single scope works", {
  tag <- htmltools::tags$div() |> alpine_id("menu")
  html <- as.character(tag)
  expect_true(grepl("x-id=", html))
  expect_true(grepl("menu", html))
})

test_that("alpine_id requires at least one scope name", {
  expect_error(
    htmltools::tags$div() |> alpine_id(),
    "at least one"
  )
})

test_that("alpine_id injects Alpine dependency", {
  tag <- htmltools::tags$div() |> alpine_id("scope")
  expect_true(is_alpine_attached(tag))
})

# ---- alpine_watch ----

test_that("alpine_watch generates $watch expression", {
  expr <- alpine_watch("count", "value => console.log(value)")
  expect_true(inherits(expr, "JS_EVAL"))
  expect_true(grepl("\\$watch", as.character(expr)))
  expect_true(grepl("'count'", as.character(expr)))
})

test_that("alpine_watch validates property", {
  expect_error(alpine_watch(123, "cb"), "character")
})

test_that("alpine_watch validates callback", {
  expect_error(alpine_watch("prop", 123), "character")
})

# ---- alpine_dispatch ----

test_that("alpine_dispatch generates $dispatch expression without detail", {
  expr <- alpine_dispatch("toggle")
  expect_true(inherits(expr, "JS_EVAL"))
  expect_true(grepl("\\$dispatch", as.character(expr)))
  expect_true(grepl("'toggle'", as.character(expr)))
})

test_that("alpine_dispatch includes detail when provided as list", {
  expr <- alpine_dispatch("select", list(id = 1L, name = "item"))
  str <- as.character(expr)
  expect_true(grepl("\\$dispatch", str))
  expect_true(grepl("select", str))
  expect_true(grepl("id", str))
})

test_that("alpine_dispatch accepts character detail", {
  expr <- alpine_dispatch("ping", "{ x: 1 }")
  expect_true(grepl("\\{ x: 1 \\}", as.character(expr)))
})

test_that("alpine_dispatch validates event", {
  expect_error(alpine_dispatch(123), "character")
})

# ---- alpine_next_tick ----

test_that("alpine_next_tick generates $nextTick expression", {
  expr <- alpine_next_tick("() => $refs.input.focus()")
  expect_true(inherits(expr, "JS_EVAL"))
  expect_true(grepl("\\$nextTick", as.character(expr)))
})

test_that("alpine_next_tick validates callback", {
  expect_error(alpine_next_tick(123), "character")
})

# ---- alpine_refs ----

test_that("alpine_refs generates $refs.name expression", {
  expr <- alpine_refs("searchBox")
  expect_true(inherits(expr, "JS_EVAL"))
  expect_equal(as.character(expr), "$refs.searchBox")
})

test_that("alpine_refs validates name", {
  expect_error(alpine_refs(123), "character")
})

# ---- alpine_store_js ----

test_that("alpine_store_js generates $store.name expression", {
  expr <- alpine_store_js("darkMode")
  expect_equal(as.character(expr), "$store.darkMode")
})

test_that("alpine_store_js generates $store.name.property expression", {
  expr <- alpine_store_js("darkMode", "on")
  expect_equal(as.character(expr), "$store.darkMode.on")
})

test_that("alpine_store_js validates store_name", {
  expect_error(alpine_store_js(123), "character")
})

test_that("alpine_store_js validates property", {
  expect_error(alpine_store_js("store", 123), "character")
})

# ---- alpine_el ----

test_that("alpine_el generates $el expression", {
  expr <- alpine_el()
  expect_true(inherits(expr, "JS_EVAL"))
  expect_equal(as.character(expr), "$el")
})

# ---- alpine_global_data ----

test_that("alpine_global_data returns a tagList with script", {
  result <- alpine_global_data(
    "dropdown",
    "() => ({ open: false, toggle() { this.open = !this.open } })"
  )
  html <- as.character(result)
  expect_true(grepl("<script>", html))
  expect_true(grepl("Alpine.data", html))
  expect_true(grepl("dropdown", html))
  expect_true(grepl("alpine:init", html))
})

test_that("alpine_global_data includes Alpine dependency", {
  result <- alpine_global_data("comp", "() => ({})")
  deps <- htmltools::findDependencies(result)
  dep_names <- vapply(deps, `[[`, character(1), "name")
  expect_true(any(grepl("alpine", dep_names, ignore.case = TRUE)))
})

test_that("alpine_global_data validates name", {
  expect_error(alpine_global_data(123, "() => ({})"), "character")
})

test_that("alpine_global_data validates js_definition", {
  expect_error(alpine_global_data("comp", 123), "character")
})

# ---- alpine_global_store ----

test_that("alpine_global_store returns a tagList with script", {
  result <- alpine_global_store("theme", mode = "light")
  html <- as.character(result)
  expect_true(grepl("<script>", html))
  expect_true(grepl("Alpine.store", html))
  expect_true(grepl("theme", html))
  expect_true(grepl("alpine:init", html))
})

test_that("alpine_global_store serializes R data to JSON", {
  result <- alpine_global_store("config", on = FALSE, count = 0L)
  html <- as.character(result)
  expect_true(grepl("false", html))
  expect_true(grepl("count", html))
})

test_that("alpine_global_store accepts raw JS value", {
  result <- alpine_global_store(
    "darkMode",
    htmlwidgets::JS("{ on: false, toggle() { this.on = !this.on } }")
  )
  html <- as.character(result)
  expect_true(grepl("toggle", html))
})

test_that("alpine_global_store includes Alpine dependency", {
  result <- alpine_global_store("myStore", x = 1L)
  deps <- htmltools::findDependencies(result)
  dep_names <- vapply(deps, `[[`, character(1), "name")
  expect_true(any(grepl("alpine", dep_names, ignore.case = TRUE)))
})

test_that("alpine_global_store validates name", {
  expect_error(alpine_global_store(123, x = 1L), "character")
})
