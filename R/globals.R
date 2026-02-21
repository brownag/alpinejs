#' Register a reusable Alpine.js component
#'
#' Generates a `<script>` tag that calls `Alpine.data('name', factory)` inside
#' a `document.addEventListener('alpine:init', ...)` listener. This registers a
#' named component that can be referenced by name in `x-data` attributes
#' (e.g., `x-data="dropdown"`), enabling reuse across multiple elements.
#'
#' @param name Character string. The component name used in `x-data="name"`.
#' @param js_definition Character string. A JavaScript function or arrow function
#'   expression that returns the component data object. Should be a function that
#'   returns an object literal, e.g.:
#'   `"() => ({ open: false, toggle() { this.open = !this.open } })"`.
#'
#' @return An `htmltools::tagList` containing a `<script>` tag and the Alpine.js
#'   dependency. Include the result in your page HTML before any element that
#'   references the component by name.
#'
#' @details
#' The generated script uses the `alpine:init` event to ensure registration
#' happens before Alpine.js processes the page, regardless of script load order.
#'
#' **Usage pattern:**
#' ```r
#' htmltools::tagList(
#'   alpine_global_data(
#'     "dropdown",
#'     "() => ({ open: false, toggle() { this.open = !this.open } })"
#'   ),
#'   htmltools::tags$div(
#'     htmltools::tags$button("Toggle") |> alpine_on("click", "toggle()"),
#'     htmltools::tags$div("Content") |> alpine_show("open")
#'   ) |> alpine_data_js("dropdown")
#' )
#' ```
#'
#' **init and destroy:** To include `init()` or `destroy()` lifecycle hooks,
#' include them directly in the JavaScript object definition string.
#'
#' @examples
#' \donttest{
#' alpine_global_data(
#'   "counter",
#'   "() => ({ count: 0, increment() { this.count++ } })"
#' )
#' }
#'
#' @importFrom htmltools tagList tags
#' @family Global Methods
#' @export
alpine_global_data <- function(name, js_definition) {
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a single character string")
  }
  if (!is.character(js_definition) || length(js_definition) != 1) {
    stop(
      "js_definition must be a single character string containing ",
      "a JavaScript function expression"
    )
  }

  script_content <- sprintf(
    "document.addEventListener('alpine:init', function() { Alpine.data('%s', %s); });",
    name,
    js_definition
  )

  htmltools::tagList(
    htmltools::tags$script(script_content),
    alpine_config()
  )
}

#' Register a global Alpine.js store
#'
#' Generates a `<script>` tag that calls `Alpine.store('name', data)` inside
#' a `document.addEventListener('alpine:init', ...)` listener. Global stores
#' are accessible from any Alpine component on the page via `$store.name`.
#'
#' @param name Character string. The store name used to access it as `$store.name`.
#' @param ... Named R arguments specifying the initial store state. These are
#'   automatically serialized to JSON. Supports the same value types as
#'   `alpine_data()`: logicals, numerics, characters, lists, data.frames,
#'   and raw JavaScript via `htmlwidgets::JS()`.
#'
#'   Alternatively, pass a single unnamed `htmlwidgets::JS()` value to use
#'   raw JavaScript as the store value.
#'
#' @return An `htmltools::tagList` containing a `<script>` tag and the Alpine.js
#'   dependency. Include the result in your page HTML before accessing the store.
#'
#' @details
#' **Accessing the store** in components:
#' - Read: `$store.name.property` or use `alpine_store_js("name", "property")`
#' - Write: `$store.name.property = newValue` in event handlers
#'
#' **Cross-component reactivity:** Because stores are global, any component that
#' reads a store value will reactively update when that value changes, even when
#' the change is triggered by a completely different component.
#'
#' @examples
#' \donttest{
#' # R data store
#' alpine_global_store("theme", mode = "light", fontSize = 16L)
#'
#' # Store with methods (raw JS)
#' alpine_global_store(
#'   "darkMode",
#'   htmlwidgets::JS("{ on: false, toggle() { this.on = !this.on } }")
#' )
#' }
#'
#' @importFrom htmltools tagList tags
#' @family Global Methods
#' @export
alpine_global_store <- function(name, ...) {
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a single character string")
  }

  args <- list(...)

  # If single unnamed JS argument, use it raw
  if (length(args) == 1 && is.null(names(args)) && inherits(args[[1]], "JS_EVAL")) {
    data_js <- as.character(args[[1]])
  } else {
    data_js <- serialize_r_to_json(...)
  }

  script_content <- sprintf(
    "document.addEventListener('alpine:init', function() { Alpine.store('%s', %s); });",
    name,
    data_js
  )

  htmltools::tagList(
    htmltools::tags$script(script_content),
    alpine_config()
  )
}
