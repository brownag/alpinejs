#' Computed Property: Count Filtered Items
#'
#' Helper to create a computed property that counts items matching a condition.
#' Returns a JavaScript function that filters a collection and returns the count.
#'
#' @param property Character string: name of the array property to filter
#' @param condition Character string: filter condition using `x` as the item variable
#'
#' @return An `htmlwidgets::JS` object containing a JavaScript function that
#'   returns a count when called.
#'
#' @details
#' This helper generates a function equivalent to:
#' ```javascript
#' function() {
#'   return this.{property}.filter(x => {condition}).length;
#' }
#' ```
#'
#' The condition should reference the item as `x`. For example:
#' - `!x.completed` -- count uncompleted items
#' - `x.type === 'income'` -- count income transactions
#'
#' @examples
#' \donttest{
#' htmltools::tags$div() |>
#'   alpine_data(
#'     todos = list(
#'       list(id = 1, title = "Task 1", completed = FALSE),
#'       list(id = 2, title = "Task 2", completed = TRUE)
#'     ),
#'     activeTodos = alpine_count_filtered("todos", "!x.completed"),
#'     completedTodos = alpine_count_filtered("todos", "x.completed")
#'   )
#' }
#'
#' @family Computed Properties
#' @export
alpine_count_filtered <- function(property, condition) {
  if (!is.character(property) || length(property) != 1) {
    stop("property must be a single character string")
  }
  if (!is.character(condition) || length(condition) != 1) {
    stop("condition must be a single character string")
  }
  
  js_code <- sprintf(
    "function() { return this.%s.filter(x => %s).length; }",
    property, condition
  )
  htmlwidgets::JS(js_code)
}

#' Computed Property: Sum Filtered Items
#'
#' Helper to create a computed property that sums a field across filtered items.
#' Combines filter and reduce operations into a single computed function.
#'
#' @param property Character string: name of the array property to filter
#' @param condition Character string: filter condition using `x` as the item variable
#' @param amount_field Character string: name of the field to sum
#'
#' @return An `htmlwidgets::JS` object containing a JavaScript function that
#'   returns a sum when called.
#'
#' @details
#' This helper generates a function equivalent to:
#' ```javascript
#' function() {
#'   return this.{property}
#'     .filter(x => {condition})
#'     .reduce((sum, x) => sum + x.{amount_field}, 0);
#' }
#' ```
#'
#' @examples
#' \donttest{
#' htmltools::tags$div() |>
#'   alpine_data(
#'     transactions = list(
#'       list(type = "income", amount = 1000),
#'       list(type = "expense", amount = 250),
#'       list(type = "income", amount = 500)
#'     ),
#'     totalIncome = alpine_sum_filtered("transactions", "x.type === 'income'", "amount"),
#'     totalExpense = alpine_sum_filtered("transactions", "x.type === 'expense'", "amount"),
#'     balance = alpine_compute("this.totalIncome() - this.totalExpense()")
#'   )
#' }
#'
#' @family Computed Properties
#' @export
alpine_sum_filtered <- function(property, condition, amount_field) {
  if (!is.character(property) || length(property) != 1) {
    stop("property must be a single character string")
  }
  if (!is.character(condition) || length(condition) != 1) {
    stop("condition must be a single character string")
  }
  if (!is.character(amount_field) || length(amount_field) != 1) {
    stop("amount_field must be a single character string")
  }
  
  js_code <- sprintf(
    "function() { return this.%s.filter(x => %s).reduce((sum, x) => sum + x.%s, 0); }",
    property, condition, amount_field
  )
  htmlwidgets::JS(js_code)
}

#' Computed Property: Custom Expression
#'
#' Helper to create a computed property from an arbitrary JavaScript expression.
#' Use for computed properties that don't fit the standard filter/sum patterns.
#'
#' @param expr Character string: JavaScript expression that can reference
#'   `this` and other state. Will be wrapped in a function that returns the result.
#'
#' @return An `htmlwidgets::JS` object containing a JavaScript function.
#'
#' @details
#' This is the escape hatch for computed properties that need custom logic.
#' The expression should be valid JavaScript that can reference `this`:
#'
#' - `"this.totalIncome() - this.totalExpense()"` -- arithmetic
#' - `"this.users.length > 0"` -- boolean check
#' - `"this.items.filter(x => x.active).map(x => x.name)"` -- transformation
#'
#' @examples
#' \donttest{
#' htmltools::tags$div() |>
#'   alpine_data(
#'     totalIncome = 5000,
#'     totalExpense = 2000,
#'     balance = alpine_compute("this.totalIncome - this.totalExpense"),
#'     hasItems = alpine_compute("this.balance > 0"),
#'     netPercent = alpine_compute("((this.totalIncome - this.totalExpense) / this.totalIncome * 100).toFixed(2)")
#'   )
#' }
#'
#' @family Computed Properties
#' @export
alpine_compute <- function(expr) {
  if (!is.character(expr) || length(expr) != 1) {
    stop("expr must be a single character string")
  }
  
  js_code <- sprintf("function() { return %s; }", expr)
  htmlwidgets::JS(js_code)
}
