#' Watch a reactive property for changes
#'
#' Generates a `$watch('property', callback)` JavaScript expression for use in
#' `alpine_init()` or event handlers. When the named property changes, Alpine.js
#' calls the provided callback with the new value.
#'
#' @param property Character string. The name of the reactive property to watch.
#'   Supports dot notation for nested properties (e.g., `"user.name"`).
#' @param callback Character string. A JavaScript function expression receiving
#'   the new value (e.g., `"value => console.log(value)"` or
#'   `"(value, oldValue) => doSomething(value, oldValue)"`).
#'
#' @return An `htmlwidgets::JS` object containing the `$watch(...)` expression.
#'
#' @details
#' Typically used inside `alpine_init()` to set up watchers when the component
#' initializes:
#'
#' ```r
#' alpine_init(tag, alpine_watch("query", "value => fetchResults(value)"))
#' ```
#'
#' The callback receives two arguments: the new value and the old value.
#' For deep watching of nested objects, Alpine.js returns the entire watched
#' object (not just the changed sub-property).
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$input(type = "text") |> alpine_model("query")
#' ) |>
#'   alpine_init(alpine_watch("query", "value => console.log('Query:', value)")) |>
#'   alpine_data(query = "")
#' }
#'
#' @family Magic Properties
#' @export
alpine_watch <- function(property, callback) {
  if (!is.character(property) || length(property) != 1) {
    stop("property must be a single character string")
  }
  if (!is.character(callback) || length(callback) != 1) {
    stop("callback must be a single character string containing a JavaScript function expression")
  }
  htmlwidgets::JS(sprintf("$watch('%s', %s)", property, callback))
}

#' Dispatch a custom browser event
#'
#' Generates a `$dispatch('event', detail)` JavaScript expression that dispatches
#' a custom DOM event from the current element. Other components or elements can
#' listen for these events using `alpine_on()`.
#'
#' @param event Character string. The event name to dispatch (e.g., `"toggle"`,
#'   `"item-selected"`, `"form-submitted"`).
#' @param detail Optional. Event detail data to include. Can be:
#'   - `NULL` (default): no detail payload
#'   - A named list: serialized to a JSON object
#'   - A character string: used as raw JavaScript
#'
#' @return An `htmlwidgets::JS` object containing the `$dispatch(...)` expression.
#'
#' @details
#' `$dispatch()` emits a `CustomEvent` that bubbles up the DOM. Parent elements
#' can listen for it with `alpine_on()`:
#'
#' ```r
#' # Emitter
#' alpine_on(button, "click", alpine_dispatch("item-selected", list(id = 1)))
#'
#' # Listener on a parent element
#' alpine_on(parent_div, "item-selected.window", "handleSelection($event.detail)")
#' ```
#'
#' Note: events dispatched with `$dispatch` bubble up the DOM. To listen outside
#' the component tree, use the `.window` modifier on `alpine_on()`.
#'
#' @examples
#' \donttest{
#' htmltools::tags$button("Select Item") |>
#'   alpine_on("click", alpine_dispatch("item-selected", list(id = 42, name = "Widget")))
#' }
#'
#' @importFrom jsonlite toJSON
#' @family Magic Properties
#' @export
alpine_dispatch <- function(event, detail = NULL) {
  if (!is.character(event) || length(event) != 1) {
    stop("event must be a single character string")
  }

  if (is.null(detail)) {
    return(htmlwidgets::JS(sprintf("$dispatch('%s')", event)))
  }

  if (is.character(detail) && length(detail) == 1) {
    detail_str <- detail
  } else if (is.list(detail) || is.numeric(detail) || is.logical(detail)) {
    detail_str <- jsonlite::toJSON(detail, auto_unbox = TRUE)
  } else {
    stop("detail must be NULL, a character string, or a named list")
  }

  htmlwidgets::JS(sprintf("$dispatch('%s', %s)", event, detail_str))
}

#' Execute code after Alpine has updated the DOM
#'
#' Generates a `$nextTick(callback)` JavaScript expression that defers execution
#' of a callback until after Alpine.js has finished processing the current reactive
#' update cycle and updated the DOM. Useful when you need to access DOM elements
#' or measurements after a state change.
#'
#' @param callback Character string. A JavaScript function expression to execute
#'   after the next DOM update (e.g., `"() => $refs.input.focus()"` or
#'   `"() => doSomethingWithUpdatedDOM()"`).
#'
#' @return An `htmlwidgets::JS` object containing the `$nextTick(...)` expression.
#'
#' @details
#' `$nextTick` is equivalent to Vue's `$nextTick`. When reactive data changes,
#' Alpine.js batches DOM updates. `$nextTick` ensures your callback runs after
#' the batch completes, so the DOM reflects the latest state.
#'
#' @examples
#' \donttest{
#' htmltools::tags$button("Focus Input") |>
#'   alpine_on("click", alpine_next_tick("() => $refs.myInput.focus()"))
#' }
#'
#' @family Magic Properties
#' @export
alpine_next_tick <- function(callback) {
  if (!is.character(callback) || length(callback) != 1) {
    stop("callback must be a single character string containing a JavaScript function expression")
  }
  htmlwidgets::JS(sprintf("$nextTick(%s)", callback))
}

#' Reference a DOM element by its x-ref name
#'
#' Generates a `$refs.name` JavaScript expression for accessing a DOM element
#' that was labeled with `alpine_ref()`. Use the resulting expression in event
#' handlers, `alpine_bind()`, or `alpine_init()` to interact with the element.
#'
#' @param name Character string. The reference name assigned via `alpine_ref()`.
#'
#' @return An `htmlwidgets::JS` object containing the `$refs.name` expression.
#'
#' @details
#' `$refs` is an object on every Alpine component that maps reference names to
#' their DOM elements. You can call any standard DOM method on the result:
#' `$refs.name.value`, `$refs.name.focus()`, `$refs.name.scrollIntoView()`, etc.
#'
#' @examples
#' \donttest{
#' htmltools::tagList(
#'   htmltools::tags$input(type = "text") |> alpine_ref("myInput"),
#'   htmltools::tags$button("Focus") |>
#'     alpine_on("click", paste0(alpine_refs("myInput"), ".focus()"))
#' ) |>
#'   (\(x) htmltools::tags$div(x))() |>
#'   alpine_data(x = "")
#' }
#'
#' @family Magic Properties
#' @export
alpine_refs <- function(name) {
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a single character string")
  }
  htmlwidgets::JS(sprintf("$refs.%s", name))
}

#' Access a global Alpine store value
#'
#' Generates a `$store.name` or `$store.name.property` JavaScript expression for
#' reading from or writing to a global Alpine store registered with
#' `alpine_global_store()`. Use the result in directives, event handlers, and
#' expressions throughout any Alpine component on the page.
#'
#' @param store_name Character string. The name of the store (as passed to
#'   `alpine_global_store()`).
#' @param property Optional character string. A property path within the store
#'   (e.g., `"on"`, `"theme"`, `"user.name"`). If `NULL`, returns the store root.
#'
#' @return An `htmlwidgets::JS` object containing the `$store.name[.property]` expression.
#'
#' @details
#' Store access expressions can be used anywhere Alpine expressions are accepted:
#' - In conditions: `alpine_show(div, alpine_store_js("app", "isLoggedIn"))`
#' - In event handlers: `alpine_on(btn, "click", "$store.darkMode.on = !$store.darkMode.on")`
#'
#' Unlike local component data, stores are globally accessible across all Alpine
#' components on the page.
#'
#' @examples
#' \donttest{
#' # Access store root
#' alpine_store_js("darkMode")
#'
#' # Access a store property
#' alpine_store_js("darkMode", "on")
#' }
#'
#' @family Magic Properties
#' @export
alpine_store_js <- function(store_name, property = NULL) {
  if (!is.character(store_name) || length(store_name) != 1) {
    stop("store_name must be a single character string")
  }

  if (is.null(property)) {
    return(htmlwidgets::JS(sprintf("$store.%s", store_name)))
  }

  if (!is.character(property) || length(property) != 1) {
    stop("property must be NULL or a single character string")
  }

  htmlwidgets::JS(sprintf("$store.%s.%s", store_name, property))
}

#' Access the current DOM element
#'
#' Returns the `$el` magic property string, which refers to the root DOM element
#' of the current Alpine.js component. Use it in expressions wherever a direct
#' reference to the component's root node is needed.
#'
#' @return An `htmlwidgets::JS` object containing `"$el"`.
#'
#' @details
#' `$el` is available in all Alpine expressions and refers to the element that
#' has `x-data`. Common uses include reading dimensions, scrolling the component
#' into view, or passing the element to a third-party library.
#'
#' @examples
#' \donttest{
#' # Scroll the component into view on a button click
#' htmltools::tags$button("Scroll Here") |>
#'   alpine_on("click", paste0(alpine_el(), ".scrollIntoView()"))
#' }
#'
#' @family Magic Properties
#' @export
alpine_el <- function() {
  htmlwidgets::JS("$el")
}
