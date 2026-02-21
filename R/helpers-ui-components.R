#' Alpine Button with Event Handler
#'
#' Creates a button with automatic event binding, styling, and optional state binding.
#' Simplifies the common pattern of buttons that trigger Alpine actions.
#'
#' @param label Character: button text label
#' @param on_click Character: JavaScript expression to execute on click
#'   (e.g., `"addTodo()"`, `"isOpen = !isOpen"`)
#' @param disabled_expr Character (optional): JavaScript expression for button disabled state
#'   (e.g., `"!finalName"`, `"isSubmitting"`)
#' @param button_class Character (optional): CSS classes for styling
#'   (e.g., `"btn btn-primary"`)
#' @param button_style Character (optional): Inline CSS styles
#' @param type Character: button type (default: `"button"`)
#'
#' @return An htmltools button tag with Alpine bindings
#'
#' @details
#' This helper reduces repetitive button code like:
#' ```r
#' tags$button("Add") |>
#'   alpine_on("click", "addItem()") |>
#'   alpine_bind("disabled", "!form.name")
#' ```
#'
#' To:
#' ```r
#' alpine_button("Add", "addItem()", disabled_expr = "!form.name")
#' ```
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   alpine_button("Submit", "handleSubmit()"),
#'   alpine_button(
#'     "Add Item",
#'     "addItem()",
#'     disabled_expr = "!itemName.trim()"
#'   )
#' ) |>
#'   alpine_data(itemName = "", handleSubmit = htmlwidgets::JS("function() { console.log('submitted'); }"))
#' }
#'
#' @family UI Components
#' @export
alpine_button <- function(label, on_click, disabled_expr = NULL, 
                          button_class = NULL, button_style = NULL, 
                          type = "button") {
  if (!is.character(label) || length(label) != 1) {
    stop("label must be a single character string")
  }
  if (!is.character(on_click) || length(on_click) != 1) {
    stop("on_click must be a single character string")
  }
  
  btn <- htmltools::tags$button(
    label,
    type = type,
    class = button_class,
    style = button_style
  ) |>
    alpine_on("click", on_click)
  
  if (!is.null(disabled_expr)) {
    btn <- btn |>
      alpine_bind("disabled", disabled_expr)
  }
  
  btn
}

#' Static Value Display (Alpine Binding)
#'
#' Creates a read-only display of an Alpine state value with formatting.
#'
#' @param value_expr Character: JavaScript expression to display
#'   (e.g., `"count"`, `"itemTotal.toFixed(2)"`)
#' @param prefix Character (optional): Text to prepend (e.g., `"$"` for currency)
#' @param suffix Character (optional): Text to append (e.g., `" items"`)
#' @param container_tag Character (optional): HTML tag for display (default: `"div"`)
#' @param style Character (optional): CSS styles
#' @param format_func Character (optional): JavaScript formatting function name.
#'   If provided, applied to value (e.g., `"toFixed"` -> `value.toFixed(2)`)
#'
#' @return An htmltools tag with text binding
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   alpine_value_display("count", suffix = " items"),
#'   htmltools::tags$button("Increment") |> alpine_on("click", "count++")
#' ) |>
#'   alpine_data(count = 5)
#' }
#'
#' @family UI Components
#' @export
alpine_value_display <- function(value_expr, prefix = NULL, suffix = NULL,
                                  container_tag = "div", style = NULL,
                                  format_func = NULL) {
  if (!is.character(value_expr) || length(value_expr) != 1) {
    stop("value_expr must be a single character string")
  }
  
  # Build the expression
  expr <- value_expr
  if (!is.null(format_func)) {
    expr <- sprintf("%s.%s", expr, format_func)
  }
  
  # Add prefix/suffix
  if (!is.null(prefix) || !is.null(suffix)) {
    prefix_str <- if (!is.null(prefix)) sprintf("'%s' + ", prefix) else ""
    suffix_str <- if (!is.null(suffix)) sprintf(" + '%s'", suffix) else ""
    expr <- paste0(prefix_str, "(", expr, ")", suffix_str)
  }
  
  # Create tag with binding
  tag_fn <- htmltools::tags[[container_tag]]
  if (is.null(tag_fn)) {
    stop(sprintf("Unknown HTML tag: %s", container_tag))
  }
  tag_fn(style = style) |>
    alpine_text(expr)
}

#' Conditional Display Helper
#'
#' Simplifies common conditional display patterns where content shows/hides
#' based on a single condition.
#'
#' @param content htmltools tag or character: content to display conditionally
#' @param show_when Character: JavaScript expression (shown when truthy)
#' @param hide_when Character (optional): Alternative - shown when falsy
#' @param empty_message Character (optional): Message to show when condition is false
#' @param container_style Character (optional): CSS for container
#'
#' @return An htmltools tag with conditional visibility
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   alpine_when(
#'     htmltools::div("Loading..."),
#'     show_when = "isLoading"
#'   ),
#'   alpine_when(
#'     htmltools::div("Done!", style = "color: green;"),
#'     show_when = "isSuccess",
#'     empty_message = "Not yet completed"
#'   )
#' ) |>
#'   alpine_data(isLoading = FALSE, isSuccess = FALSE)
#' }
#'
#' @family Core Directives
#' @export
alpine_when <- function(content, show_when = NULL, hide_when = NULL,
                        empty_message = NULL, container_style = NULL) {
  if (is.null(show_when) && is.null(hide_when)) {
    stop("Either show_when or hide_when must be provided")
  }
  
  condition <- show_when %||% paste0("!(", hide_when, ")")
  
  # Return condition may be a tag with alpine_show already
  if (inherits(content, "shiny.tag")) {
    result <- content |> alpine_show(condition)
  } else {
    result <- htmltools::div(
      content,
      style = container_style
    ) |>
      alpine_show(condition)
  }
  
  # Add empty state if provided
  if (!is.null(empty_message)) {
    opposite_condition <- if (!is.null(show_when)) {
      paste0("!(", show_when, ")")
    } else {
      hide_when
    }
    
    empty_div <- htmltools::div(
      empty_message,
      style = container_style
    ) |>
      alpine_show(opposite_condition)
    
    result <- list(result, empty_div)
  }
  
  result
}

# Helper for %||% operator (NULL coalescing)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
