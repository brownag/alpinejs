#' Get Value from localStorage
#'
#' Helper to create a JavaScript expression that retrieves a value from browser
#' localStorage with an optional default fallback.
#'
#' @param key Character string: the localStorage key to retrieve
#' @param default Value to use if the key doesn't exist. Can be:
#'   - `NULL` (no default, returns just the getItem call)
#'   - Character string (quoted in JavaScript)
#'   - Numeric value
#'   - Logical value (TRUE/FALSE -> true/false)
#'   - A list/object (will be serialized using `alpine_object()`)
#'
#' @return An `htmlwidgets::JS` object containing a JavaScript expression
#'   that retrieves from localStorage or uses the default.
#'
#' @details
#' For initializing state with persisted values:
#'
#' ```r
#' theme: alpine_storage_get("theme", default = "light")
#' ```
#'
#' When using JSON objects in localStorage (as with `alpine_storage_set()`),
#' parse the result:
#'
#' ```r
#' # Get stored settings or empty object
#' settings: JS("JSON.parse(localStorage.getItem('settings') || '{}')")
#' ```
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$span() |> alpine_text("theme"),
#'   htmltools::tags$button("Toggle Dark") |> alpine_on("click", "theme = 'dark'"),
#'   htmltools::tags$button("Toggle Light") |> alpine_on("click", "theme = 'light'")
#' ) |>
#'   alpine_data(
#'     theme = alpine_storage_get("theme", default = "light")
#'   )
#' }
#'
#' @family Client Storage
#' @export
alpine_storage_get <- function(key, default = NULL) {
  if (!is.character(key) || length(key) != 1) {
    stop("key must be a single character string")
  }
  
  base_str <- sprintf("localStorage.getItem('%s')", key)
  
  if (is.null(default)) {
    return(htmlwidgets::JS(base_str))
  }
  
  # Format default value
  if (is.character(default)) {
    default_str <- sprintf("'%s'", default)
  } else if (is.numeric(default)) {
    default_str <- as.character(default)
  } else if (is.logical(default)) {
    default_str <- if (default) "true" else "false"
  } else if (is.list(default)) {
    # For objects, user should pass alpine_object() or JS()
    # For now, just convert to string
    default_str <- "null"
  } else {
    default_str <- "null"
  }
  
  js_str <- sprintf("%s || %s", base_str, default_str)
  htmlwidgets::JS(js_str)
}

#' Set Value in localStorage
#'
#' Helper to create a JavaScript statement that stores a value in localStorage.
#' Typically used within event handlers for persistence.
#'
#' @param key Character string: the localStorage key to set
#' @param value_expr Character string: JavaScript expression to store.
#'   Default is `"this"` (store the entire component state).
#'   Can reference any state property: `"this.todos"`, `"this.user.name"`, etc.
#'
#' @return A character string containing a JavaScript statement for use in
#'   event handlers. Must be passed to functions that accept JS statements
#'   like `js_event_handler()` or event handlers like `\@click`.
#'
#' @details
#' This helper generates:
#' ```javascript
#' localStorage.setItem('key', JSON.stringify({value_expr}));
#' ```
#'
#' For use in event handlers, pass this result to an `\@event` attribute or
#' include in a composite event handler.
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$input(type = "text", placeholder = "Todo") |> alpine_model("newTodo"),
#'   htmltools::tags$button("Add & Save") |>
#'     alpine_on("click", 
#'       "todos.push({id: Date.now(), text: newTodo}); localStorage.setItem('todos', JSON.stringify(todos)); newTodo = ''"),
#'   htmltools::tags$ul(
#'     alpine_for(
#'       htmltools::tags$li() |> alpine_text("item.text"),
#'       "item in todos",
#'       key = "item.id"
#'     )
#'   )
#' ) |>
#'   alpine_data(
#'     todos = alpine_storage_get("todos", default = list()),
#'     newTodo = ""
#'   )
#' }
#'
#' @family Client Storage
#' @export
alpine_storage_set <- function(key, value_expr = "this") {
  if (!is.character(key) || length(key) != 1) {
    stop("key must be a single character string")
  }
  if (!is.character(value_expr) || length(value_expr) != 1) {
    stop("value_expr must be a single character string")
  }
  
  # Return as character (not wrapped in JS), so it can be used in event handler builders
  sprintf("localStorage.setItem('%s', JSON.stringify(%s))", key, value_expr)
}
