#' Create a JavaScript empty array for Alpine.js state
#'
#' Helper that creates an empty JavaScript array `[]` without writing raw JavaScript.
#' Use as an initial value in `alpine_data()` when you want to start with an empty list.
#'
#' @return An `htmlwidgets::JS` object containing an empty JSON array.
#'   Suitable for use as an initial value in `alpine_data()`.
#'
#' @details
#' This function is equivalent to `htmlwidgets::JS("[]")` but signals
#' intent more clearly and reads more naturally in R code.
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$button("Add Todo") |> alpine_on("click", "todos.push({id: Date.now(), text: newTodo, done: false}); newTodo = ''"),
#'   htmltools::tags$input(type = "text", placeholder = "New todo") |> alpine_model("newTodo")
#' ) |>
#'   alpine_data(
#'     todos = alpine_array(),
#'     newTodo = ""
#'   )
#' }
#'
#' @family JavaScript Helpers
#' @export
alpine_array <- function() {
  htmlwidgets::JS("[]")
}

#' Create a JavaScript object for Alpine.js state
#'
#' Helper that builds a JavaScript object literal from R named arguments,
#' without writing raw JavaScript strings. Automatically serializes R values to JSON.
#'
#' @param ... Named arguments representing properties of the JavaScript object.
#'   Values are automatically serialized to JSON. Supports:
#'   - Character strings (quoted in JSON)
#'   - Numeric values (integers and floats)
#'   - Logical values (converted to true/false)
#'   - NULL (converted to null)
#'   - Lists as nested objects
#'   - `htmlwidgets::JS()` expressions for raw JavaScript injection
#'
#' @return An `htmlwidgets::JS` object containing a JavaScript object literal.
#'   Suitable for use as an initial value in `alpine_data()`.
#'
#' @details
#' This function provides a semantic alternative to multi-line JavaScript strings
#' for defining Alpine component objects. Use R syntax to define objects:
#'
#' ```r
#' # Before
#' settings = htmlwidgets::JS("{
#'   username: 'john_doe',
#'   email: 'john@example.com',
#'   theme: 'light'
#' }")
#'
#' # After
#' settings = alpine_object(
#'   username = "john_doe",
#'   email = "john@example.com",
#'   theme = "light"
#' )
#' ```
#'
#' For raw JavaScript expressions (like computed properties), pass them
#' wrapped in `htmlwidgets::JS()`:
#'
#' ```r
#' alpine_object(
#'   timestamp = htmlwidgets::JS("Date.now()"),
#'   isActive = TRUE
#' )
#' ```
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$input(type = "text", placeholder = "Username") |> alpine_model("user.name"),
#'   htmltools::tags$input(type = "email", placeholder = "Email") |> alpine_model("user.email"),
#'   htmltools::tags$input(type = "number", placeholder = "Font size") |> alpine_model("settings.fontSize", .number = TRUE)
#' ) |>
#'   alpine_data(
#'     user = alpine_object(
#'       name = "Alice",
#'       email = "alice@example.com",
#'       premium = TRUE
#'     ),
#'     settings = alpine_object(
#'       theme = "light",
#'       fontSize = 16
#'     )
#'   )
#' }
#'
#' @family JavaScript Helpers
#' @export
alpine_object <- function(...) {
  args <- list(...)
  
  # If no arguments, return empty object
  if (length(args) == 0) {
    return(htmlwidgets::JS("{}"))
  }
  
  # Get argument names
  arg_names <- names(args)
  if (is.null(arg_names) || any(arg_names == "")) {
    stop("All arguments to alpine_object() must be named")
  }
  
  # Separate JS and non-JS values
  is_js <- vapply(args, function(x) {
    inherits(x, "JS_EVAL") || inherits(x, "html")
  }, logical(1))
  
  json_parts <- args[!is_js]
  js_parts <- args[is_js]
  
  # Serialize non-JS parts
  if (length(json_parts) > 0) {
    # Pre-process special types
    json_parts <- lapply(json_parts, convert_r_type_for_json)
    
    # Serialize to JSON
    json_str <- jsonlite::toJSON(
      json_parts,
      auto_unbox = TRUE,
      na = "null",
      digits = NA,
      pretty = FALSE
    )
    
    # Remove outer brackets (toJSON wraps in [...] for unnamed objects)
    if (grepl("^\\[", json_str) && grepl("\\]$", json_str)) {
      json_str <- substr(json_str, 2, nchar(json_str) - 1)
    }
  } else {
    json_str <- ""
  }
  
  # Handle JS parts
  if (length(js_parts) > 0) {
    js_values <- vapply(js_parts, function(x) {
      if (inherits(x, "JS_EVAL")) {
        js_str <- as.character(x)
      } else if (inherits(x, "html")) {
        js_str <- as.character(x)
      } else {
        js_str <- ""
      }
      # Minify
      js_str <- gsub("\n", " ", js_str, fixed = TRUE)
      js_str <- gsub("\r", " ", js_str, fixed = TRUE)
      js_str <- gsub("  +", " ", js_str)
      trimws(js_str)
    }, character(1))
    
    js_names <- names(js_parts)
    js_pairs <- sprintf("\"%s\":%s", js_names, js_values)
    
    # Merge JSON and JS parts
    if (nchar(json_str) > 0) {
      json_str <- sub("}\\s*$", "", json_str)
      json_str <- paste0(json_str, ",", paste(js_pairs, collapse = ","), "}")
    } else {
      json_str <- paste0("{", paste(js_pairs, collapse = ","), "}")
    }
  } else {
    # Ensure object braces
    if (!nchar(json_str)) {
      json_str <- "{}"
    } else if (substr(json_str, 1, 1) != "{") {
      json_str <- paste0("{", json_str, "}")
    }
  }
  
  htmlwidgets::JS(json_str)
}
