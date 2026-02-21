#' Email Validator
#'
#' Helper to create a validation function for email addresses.
#' Returns a JavaScript function that tests if a field contains a valid email.
#'
#' @param field Character string: name of the field to validate (default: "email")
#'
#' @return An `htmlwidgets::JS` object containing a JavaScript validation function
#'   that returns `TRUE` if the field is a valid email, `FALSE` otherwise.
#'
#' @details
#' This creates a function equivalent to:
#' ```javascript
#' function() {
#'   const re = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
#'   return re.test(this.{field});
#' }
#' ```
#'
#' The regex requires: characters@characters.characters (basic email validation)
#'
#' @examples
#' \donttest{
#' htmltools::tags$div() |>
#'   alpine_data(
#'     email = "",
#'     isValidEmail = alpine_validate_email("email")
#'   )
#' }
#'
#' @family Input Validators
#' @export
alpine_validate_email <- function(field = "email") {
  if (!is.character(field) || length(field) != 1) {
    stop("field must be a single character string")
  }
  
  js_code <- sprintf(
    "function() { const re = /^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$/; return re.test(this.%s); }",
    field
  )
  htmlwidgets::JS(js_code)
}

#' Length Validator Expression
#'
#' Helper to create a validation condition for string or array length.
#' Returns a character string containing a JavaScript expression (not a function),
#' suitable for use with `alpine_multi_condition()` or `alpine_compute()`.
#'
#' @param field Character string: name of the field to validate
#' @param min Numeric: minimum length (default: 1)
#' @param max Numeric: maximum length (default: Inf for no limit)
#'
#' @return A character string containing a JavaScript expression. Can be used
#'   directly in `alpine_multi_condition()` or wrapped with `alpine_compute()`.
#'
#' @details
#' This generates expressions like:
#' - `"this.message.length >= 1"` (minimum only)
#' - `"this.message.length <= 500"` (maximum only)
#' - `"this.message.length >= 1 && this.message.length <= 500"` (both)
#'
#' @examples
#' \donttest{
#' htmltools::tags$div() |>
#'   alpine_data(
#'     fullName = "",
#'     isValidEmail = alpine_validate_email("email"),
#'     message = "",
#'     isFormValid = alpine_multi_condition(
#'       "this.fullName.length > 0",
#'       "this.isValidEmail()",
#'       alpine_validate_length("message", min = 1, max = 500)
#'     )
#'   )
#' }
#'
#' @family Input Validators
#' @export
alpine_validate_length <- function(field, min = 1, max = Inf) {
  if (!is.character(field) || length(field) != 1) {
    stop("field must be a single character string")
  }
  if (!is.numeric(min) || length(min) != 1) {
    stop("min must be a single numeric value")
  }
  if (!is.numeric(max) || length(max) != 1) {
    stop("max must be a single numeric value")
  }
  
  parts <- c()
  
  if (!is.infinite(min) && min > 0) {
    parts <- c(parts, sprintf("this.%s.length >= %d", field, min))
  }
  
  if (!is.infinite(max)) {
    parts <- c(parts, sprintf("this.%s.length <= %d", field, max))
  }
  
  if (length(parts) == 0) {
    # No constraints
    "true"
  } else {
    paste(parts, collapse = " && ")
  }
}

#' Multi-Condition Validator
#'
#' Helper to combine multiple validation conditions into a single function.
#' Useful for complex form validation with multiple interdependent checks.
#'
#' @param ... Character strings representing JavaScript expressions or results
#'   from other helper functions like `alpine_validate_length()` or direct expressions
#' @param operator Character string: logical operator to use ("&&" or "||", default: "&&")
#'
#' @return An `htmlwidgets::JS` object containing a JavaScript function that
#'   evaluates all conditions and returns the combined result.
#'
#' @details
#' Conditions are combined with the specified operator. Default is `&&` (AND),
#' meaning all conditions must be true. Use `||` (OR) to require at least one.
#'
#' Conditions can be:
#' - Simple field checks: `"this.name.length > 0"`
#' - Method calls: `"this.isValidEmail()"`
#' - Expression results: output from `alpine_validate_length()`
#' - Logical comparisons: `"!this.submitted"`
#'
#' @examples
#' \donttest{
#' htmltools::tags$div() |>
#'   alpine_data(
#'     notifications = TRUE,
#'     pushNotif = TRUE,
#'     smsNotif = FALSE,
#'     hasNotifications = alpine_multi_condition(
#'       "this.notifications",
#'       "this.pushNotif",
#'       "this.smsNotif",
#'       operator = "||"
#'     )
#'   )
#' }
#'
#' @family Computed Properties
#' @export
alpine_multi_condition <- function(..., operator = "&&") {
  conditions <- list(...)
  
  if (length(conditions) == 0) {
    stop("At least one condition must be provided")
  }
  
  if (!is.character(operator) || length(operator) != 1) {
    stop("operator must be a single character string")
  }
  
  # Flatten conditions (in case some are lists)
  conditions <- unlist(conditions)
  
  # Combine with operator
  condition_str <- paste(conditions, collapse = paste0(" ", operator, " "))
  
  js_code <- sprintf("function() { return %s; }", condition_str)
  htmlwidgets::JS(js_code)
}
