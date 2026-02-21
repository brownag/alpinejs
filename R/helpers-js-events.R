#' Add Item to Array
#'
#' Helper to create a JavaScript function that adds an item to an array with
#' optional validation and field clearing.
#'
#' @param array Character: path to the array property (e.g., `"products"`, `"todos"`)
#' @param item Character: JavaScript object literal representing the item to add
#'   (e.g., `"{ id: this.nextId++, name: this.newName }"`)
#' @param validation Character (optional): validation condition that must be true
#'   to add (e.g., `"this.newName && this.newName.trim()"`)
#' @param clear_statements Character (optional): JavaScript statement(s) to execute
#'   after adding (e.g., `"this.newName = ''"`). Can include multiple statements
#'   separated by semicolons.
#'
#' @return A character string containing the JavaScript function body.
#'   Use with `htmlwidgets::JS(sprintf("function() { %s }", ...))` for event handlers.
#'
#' @details
#' This helper generates common add-to-list patterns like in the Product Inventory
#' example where items are validated, added, and form fields are cleared.
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$input(type = "text", placeholder = "Product name") |> alpine_model("name"),
#'   htmltools::tags$input(type = "number", placeholder = "Price", value = 0) |> alpine_model("price", .number = TRUE),
#'   htmltools::tags$button("Add") |>
#'     alpine_on("click",
#'       alpine_add_to_array(
#'         "products",
#'         "{ id: Math.random(), name: this.name, price: this.price }",
#'         "this.name && this.price > 0",
#'         "this.name = ''; this.price = 0"
#'       )
#'     ),
#'   htmltools::tags$ul(
#'     alpine_for(
#'       htmltools::tags$li() |> alpine_text("`${item.name}: $${item.price}`"),
#'       "item in products",
#'       key = "item.id"
#'     )
#'   )
#' ) |>
#'   alpine_data(
#'     products = alpine_array(),
#'     name = "",
#'     price = 0
#'   )
#' }
#'
#' @family Array Operations
#' @export
alpine_add_to_array <- function(array, item, validation = NULL, clear_statements = NULL) {
  if (!is.character(array) || length(array) != 1) {
    stop("array must be a single character string")
  }
  if (!is.character(item) || length(item) != 1) {
    stop("item must be a single character string")
  }
  
  statements <- c()
  
  # Add validation check if provided
  if (!is.null(validation)) {
    statements <- c(statements, sprintf("if (!(%s)) return;", validation))
  }
  
  # Add item to array
  statements <- c(statements, sprintf("this.%s.push(%s);", array, item))
  
  # Add clear statements if provided
  if (!is.null(clear_statements)) {
    statements <- c(statements, clear_statements)
  }
  
  paste(statements, collapse = " ")
}

#' Remove Item from Array
#'
#' Helper to create a JavaScript fragment that removes an item from an array
#' by ID or index.
#'
#' @param array Character: path to the array property (e.g., `"products"`, `"todos"`)
#' @param identifier Character: property/parameter to match (e.g., `"id"`, or a parameter
#'   like `"itemId"` if the function takes a parameter)
#' @param is_param Logical: if TRUE, `identifier` is a function parameter;
#'   if FALSE, it's a property to match by ID. Default: FALSE
#'
#' @return A character string containing JavaScript code for removal.
#'   Use within a function or event handler.
#'
#' @details
#' Generates code like:
#' ```javascript
#' this.products = this.products.filter(p => p.id !== id);
#' ```
#'
#' If `is_param=FALSE`, wraps in a function that accepts the identifier.
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$ul(
#'     alpine_for(
#'       htmltools::tags$li(
#'         htmltools::tags$span() |> alpine_text("item.name"),
#'         htmltools::tags$button("Delete") |>
#'           alpine_on("click", sprintf("function() { %s }", 
#'             alpine_remove_from_array("products", "item.id", is_param = FALSE)))
#'       ),
#'       "item in products",
#'       key = "item.id"
#'     )
#'   )
#' ) |>
#'   alpine_data(
#'     products = list(
#'       list(id = 1, name = "Product A"),
#'       list(id = 2, name = "Product B")
#'     )
#'   )
#' }
#'
#' @family Array Operations
#' @export
alpine_remove_from_array <- function(array, identifier = "id", is_param = FALSE) {
  if (!is.character(array) || length(array) != 1) {
    stop("array must be a single character string")
  }
  if (!is.character(identifier) || length(identifier) != 1) {
    stop("identifier must be a single character string")
  }
  
  # Use short variable name for array items (first letter of singular array name)
  array_var <- substr(array, 1, 1)
  
  if (is_param) {
    # identifier is a function parameter
    sprintf(
      "this.%s = this.%s.filter(%s => %s.%s !== %s);",
      array, array, array_var, array_var, identifier, identifier
    )
  } else {
    # identifier is a property to match
    sprintf(
      "this.%s = this.%s.filter(%s => %s.%s !== valueToRemove);",
      array, array, array_var, array_var, identifier
    )
  }
}

#' Update Array Item Property
#'
#' Helper to create a JavaScript fragment that updates a property of an item
#' in an array by ID.
#'
#' @param array Character: path to the array property (e.g., `"products"`)
#' @param id_param Character: name of the parameter for item ID (e.g., `"id"`)
#' @param property Character: property to update (e.g., `"quantity"`)
#' @param value_param Character: name of the parameter for new value (default: `"newValue"`)
#'
#' @return A character string containing JavaScript code for updating.
#'   Generate the full function with sprintf wrapper.
#'
#' @details
#' Generates code like:
#' ```javascript
#' const item = this.products.find(p => p.id === id);
#' if (item && newValue >= 0) item.quantity = newValue;
#' ```
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$ul(
#'     alpine_for(
#'       htmltools::tags$li(
#'         htmltools::tags$span() |> alpine_text("item.quantity"),
#'         htmltools::tags$input(type = "number") |> alpine_model("newQty", .number = TRUE),
#'         htmltools::tags$button("Update") |>
#'           alpine_on("click",
#'             sprintf("function() { %s; if (newQty > 0) item.quantity = newQty; }",
#'               alpine_update_array_item("products", "id", "quantity"))
#'           )
#'       ),
#'       "item in products",
#'       key = "item.id"
#'     )
#'   )
#' ) |>
#'   alpine_data(
#'     products = list(list(id = 1, quantity = 5)),
#'     newQty = 0
#'   )
#' }
#'
#' @family Array Operations
#' @export
alpine_update_array_item <- function(array, id_param = "id", property = "value", 
                                      value_param = "newValue") {
  if (!is.character(array) || length(array) != 1) {
    stop("array must be a single character string")
  }
  
  array_var <- substr(array, 1, 1)
  
  sprintf(
    "const item = this.%s.find(%s => %s.%s === %s);",
    array, array_var, array_var, id_param, id_param
  )
}

#' Clear Form Fields
#'
#' Helper to generate JavaScript for resetting multiple form fields to initial values.
#'
#' @param fields Named list or character vector of field paths and their initial values.
#'   Names are paths (e.g., `"form.firstName"`), values are R objects to convert to JS.
#'
#' @return A character string with JavaScript assignment statements.
#'
#' @details
#' Useful for clearing forms after submission or adding items to a list.
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$input(type = "text", placeholder = "Name") |> alpine_model("form.name"),
#'   htmltools::tags$input(type = "number", placeholder = "Qty") |> alpine_model("form.quantity", .number = TRUE),
#'   htmltools::tags$button("Add & Clear") |>
#'     alpine_on("click", 
#'       sprintf("function() { addProduct(); %s }",
#'         alpine_clear_fields(list("form.name" = "", "form.quantity" = 0))))
#' ) |>
#'   alpine_data(
#'     form = alpine_object(name = "", quantity = 0),
#'     addProduct = htmlwidgets::JS("function() { console.log(this.form); }")
#'   )
#' }
#'
#' @family Form Field Operations
#' @export
alpine_clear_fields <- function(fields) {
  if (is.null(fields) || length(fields) == 0) {
    return("")
  }
  
  statements <- c()
  
  if (is.list(fields)) {
    for (name in names(fields)) {
      value <- fields[[name]]
      
      # Convert R value to JavaScript
      if (is.character(value)) {
        js_value <- sprintf("'%s'", value)
      } else if (is.numeric(value)) {
        js_value <- as.character(value)
      } else if (is.logical(value)) {
        js_value <- if (value) "true" else "false"
      } else if (is.list(value)) {
        # For nested objects, just represent as empty object
        js_value <- "{}"
      } else {
        js_value <- "null"
      }
      
      statements <- c(statements, sprintf("this.%s = %s;", name, js_value))
    }
  }
  
  paste(statements, collapse = " ")
}

#' Combine Multiple Validation Conditions
#'
#' Helper to create AND/OR combinations of validation conditions.
#'
#' @param conditions Character vector: JavaScript boolean expressions to combine
#' @param operator Character: either `"&&"` (all must be true) or `"||"` (at least one true).
#'   Default: `"&&"`
#'
#' @return A character string with parenthesized combined conditions.
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$input(type = "text", placeholder = "First name") |> alpine_model("firstName"),
#'   htmltools::tags$input(type = "text", placeholder = "Last name") |> alpine_model("lastName"),
#'   htmltools::tags$input(type = "email", placeholder = "Email") |> alpine_model("email"),
#'   htmltools::tags$button("Submit") |>
#'     alpine_on("click",
#'       sprintf("function() { if (%s) console.log('Valid'); else alert('Fill all fields'); }",
#'         alpine_combine_conditions(
#'           c("this.firstName", "this.lastName", "this.email"),
#'           operator = "&&"
#'         )))
#' ) |>
#'   alpine_data(firstName = "", lastName = "", email = "")
#' }
#'
#' @family Computed Properties
#' @export
alpine_combine_conditions <- function(conditions, operator = "&&") {
  if (!is.character(conditions) || length(conditions) == 0) {
    stop("conditions must be a non-empty character vector")
  }
  
  if (!operator %in% c("&&", "||")) {
    stop("operator must be either '&&' or '||'")
  }
  
  paste(paste("(", conditions, ")", sep = ""), collapse = paste0(" ", operator, " "))
}
