#' Initialize Alpine.js reactive state
#'
#' Creates an `x-data` attribute containing the initial reactive state for an Alpine.js component.
#' State is provided as R named arguments, which are automatically serialized to JSON.
#'
#' @importFrom htmltools tagAppendAttributes
#' @importFrom jsonlite toJSON
#'
#' @param tag An htmltools tag object.
#' @param ... Named arguments representing the reactive state. Can include
#'   R data structures (vectors, lists, data.frames) which are automatically
#'   converted to JSON, as well as raw JavaScript expressions wrapped in
#'   `htmlwidgets::JS()`.
#' @param .cssSelector Optional CSS selector for targeting a nested element
#'   within the tag. If provided, the attribute is applied only to the
#'   matched element. See [htmltools::tagAppendAttributes()].
#'
#' @return The modified tag with `x-data` attribute appended.
#'
#' @details
#' **Type Conversions:**
#' - Logical: `TRUE` -> `true`, `FALSE` -> `false`
#' - Factors: converted to character vectors
#' - Dates/POSIXct: converted to ISO 8601 strings
#' - NA values: converted to JSON `null`
#' - data.frame: converted to array of objects (rows)
#' - `htmlwidgets::JS("...")`: injected as raw JavaScript without quoting
#'
#' **Example:**
#' ```
#' htmltools::tags$div(htmltools::tags$button("Click me")) |>
#'   alpine_data(
#'     count = 0,
#'     items = list(a = 1, b = 2, c = 3),
#'     isOpen = FALSE,
#'     customFn = htmlwidgets::JS("() => alert('hi')")
#'   )
#' ```
#'
#' Renders as:
#' ```html
#' <div x-data='{"count":0,"items":{"a":1,"b":2,"c":3},"isOpen":false,"customFn":() => alert("hi")}'>
#'   <button>Click me</button>
#' </div>
#' ```
#'
#' @examples
#' \donttest{
#' htmltools::tags$div("Counter: @count") |>
#'   alpine_data(count = 0)
#' }
#'
#' @family Core Directives
#' @export
alpine_data <- function(tag, ..., .cssSelector = NULL) {
  # Serialize the provided data to JSON
  json_str <- serialize_r_to_json(...)

  # Append the x-data attribute
  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-data` = json_str,
    .cssSelector = .cssSelector
  )

  # Ensure Alpine.js dependency is attached
  tag <- ensure_alpine_dependency(tag)
  
  # Assign alpine_component class for S3 method dispatch
  tag <- assign_alpine_class(tag)
  
  return(tag)
}

#' Attach an event listener to an HTML element
#'
#' Adds an `@EVENT` attribute (Alpine.js event listener syntax) to an element.
#' When the specified event occurs in the browser, Alpine.js executes the provided action
#' within the component's reactive scope.
#'
#' @param tag An htmltools tag object.
#' @param event Character string specifying the event name (e.g., "click",
#'   "keydown", "change", "submit"). Standard DOM event names are supported.
#' @param action Character string containing the JavaScript action to execute.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `@EVENT` attribute appended.
#'
#' @details
#' **Modern Syntax:** Uses the `@EVENT` shorthand notation (e.g., `@click`),
#' which is equivalent to `x-on:EVENT` but more concise.
#'
#' **Action Strings:** The action is evaluated within the Alpine component's
#' scope, so it can reference state variables directly without prefixes.
#'
#' **Common Events:**
#' - Mouse: `click`, `dblclick`, `mouseenter`, `mouseleave`, `mouseover`
#' - Keyboard: `keydown`, `keyup`, `keypress`
#' - Form: `change`, `input`, `submit`, `reset`, `focus`, `blur`
#' - Touch: `touchstart`, `touchend`, `touchmove`
#'
#' @examples
#' \donttest{
#' htmltools::tags$button("Increment") |>
#'   alpine_on("click", "count++")
#' }
#'
#' @family Core Directives
#' @export
alpine_on <- function(tag, event, action, .cssSelector = NULL) {
  # Validate inputs
  if (!is.character(event) || length(event) != 1) {
    stop("event must be a single character string (e.g., 'click')")
  }
  if (!is.character(action) || length(action) != 1) {
    stop("action must be a single character string")
  }

  # Create attribute name using @ shorthand (modern syntax)
  attr_name <- paste0("@", event)
  
  # Append attribute while preserving case
  tag <- do.call(
    alpine_append_attributes,
    c(list(tag), stats::setNames(list(action), attr_name))
  )

  # Ensure Alpine.js dependency is attached
  tag <- ensure_alpine_dependency(tag)
  
  # Assign alpine_component class for S3 method dispatch
  tag <- assign_alpine_class(tag)
  
  return(tag)
}

#' Conditionally show/hide an HTML element
#'
#' Creates an `x-show` attribute that controls element visibility based on a JavaScript condition.
#' Alpine.js evaluates the condition and applies CSS display property. The element remains in the
#' DOM even when hidden, suitable for toggled UI (dropdowns, tooltips, modals).
#'
#' @param tag An htmltools tag object.
#' @param condition Character string containing a JavaScript expression that
#'   evaluates to a boolean. Can reference state variables from `alpine_data()`.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-show` attribute appended.
#'
#' @details
#' `alpine_show()` toggles CSS visibility. The element remains in the DOM.
#'
#' @examples
#' \donttest{
#' htmltools::tags$div("Content") |>
#'   alpine_show("isVisible")
#' }
#'
#' @family Core Directives
#' @export
alpine_show <- function(tag, condition, .cssSelector = NULL) {
  # Validate input
  if (!is.character(condition) || length(condition) != 1) {
    stop("condition must be a single character string")
  }

  # Append the x-show attribute
  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-show` = condition,
    .cssSelector = .cssSelector
  )

  # Ensure Alpine.js dependency is attached
  tag <- ensure_alpine_dependency(tag)
  
  # Assign alpine_component class for S3 method dispatch
  tag <- assign_alpine_class(tag)
  
  return(tag)
}

#' Dynamically bind an HTML attribute to an expression
#'
#' Creates an `x-bind:ATTRIBUTE` attribute that binds an HTML attribute to a JavaScript expression.
#' Alpine.js evaluates the expression and updates the attribute value whenever state changes.
#'
#' @param tag An htmltools tag object.
#' @param attribute Character string specifying the HTML attribute to bind
#'   (e.g., "class", "disabled", "href", "src", "style", "aria-label").
#' @param value Character string containing a JavaScript expression. Can
#'   reference state variables from `alpine_data()` and use conditional logic.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-bind:ATTR` attribute appended.
#'
#' @details
#' For class binding, use CSS class strings. For boolean attributes (disabled,
#' checked, hidden), use boolean expressions. For style binding, use JavaScript
#' object notation or CSS strings.
#'
#' @examples
#' \donttest{
#' htmltools::tags$button("Status") |>
#'   alpine_bind("class", "isActive ? 'btn-active' : 'btn-inactive'")
#' }
#'
#' @family Core Directives
#' @export
alpine_bind <- function(tag, attribute, value, .cssSelector = NULL) {
  # Validate inputs
  if (!is.character(attribute) || length(attribute) != 1) {
    stop("attribute must be a single character string (e.g., 'disabled')")
  }
  if (!is.character(value) || length(value) != 1) {
    stop("value must be a single character string")
  }

  # Create attribute name using x-bind: prefix
  attr_name <- paste0("x-bind:", attribute)
  
  # Append attribute while preserving case
  tag <- do.call(
    alpine_append_attributes,
    c(list(tag), stats::setNames(list(value), attr_name))
  )

  # Ensure Alpine.js dependency is attached
  tag <- ensure_alpine_dependency(tag)
  
  # Assign alpine_component class for S3 method dispatch
  tag <- assign_alpine_class(tag)
  
  return(tag)
}

#' Two-way data binding for form inputs
#'
#' Creates an `x-model` attribute that synchronizes a form input with a state variable.
#' When the user changes the input, Alpine.js automatically updates the state variable.
#' When the state variable changes, Alpine.js automatically updates the input display.
#'
#' @param tag An htmltools tag object, typically an `<input>`, `<textarea>`,
#'   or `<select>` element.
#' @param variable Character string specifying the state variable name to bind
#'   to (e.g., "searchText", "selectedOption").
#' @param .lazy Logical. If `TRUE`, updates only on blur/change events instead
#'   of on every input. Useful for validation or expensive operations.
#' @param .debounce Numeric or `NULL`. Debounces updates by specified milliseconds.
#' @param .number Logical. If `TRUE`, converts input value to number.
#' @param .trim Logical. If `TRUE`, removes leading/trailing whitespace.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-model` attribute appended.
#'
#' @details
#' **Input Types:**
#' - Text inputs (`<input type="text">`): binds to string value
#' - Checkboxes (`<input type="checkbox">`): binds to boolean
#' - Radio buttons: binds to selected value
#' - Selects (`<select>`): binds to (first) selected option value
#' - Textareas: binds to text content
#' - Multiple selects: binds to array of selected values
#'
#' **Modifier Syntax:** Modifiers are applied in the correct Alpine.js order:
#' `.lazy` -> `.debounce.NNN` -> `.number` -> `.trim`. This means an expression
#' `alpine_model(input(), "count", .lazy = TRUE, .debounce = 300, .number = TRUE)`
#' produces `x-model.lazy.debounce.300.number="count"`.
#'
#' **Modifier Combinations:** Modifiers can be combined. Common patterns:
#' - `.lazy = TRUE`: For form validation (update only on blur)
#' - `.debounce = 300`: For search inputs (avoid queries on every keystroke)
#' - `.number = TRUE`: For numeric inputs without type="number"
#' - `.lazy + .debounce`: For expensive validation (throttle + debounce)
#' - `.debounce + .number`: For numeric search/filter inputs
#'
#' **Performance:** Debouncing is especially useful for inputs that trigger
#' expensive operations like API calls, regex searches, or large dataset
#' filtering. Recommended starting values: 300-500ms for user typing.
#'
#' @examples
#' \donttest{
#' htmltools::tags$input(type = "text", placeholder = "Enter name") |>
#'   alpine_model("userName")
#' }
#'
#' @family Form Input
#' @export
alpine_model <- function(
  tag,
  variable,
  .lazy = FALSE,
  .debounce = NULL,
  .number = FALSE,
  .trim = FALSE,
  .cssSelector = NULL
) {
  # Validate input
  if (!is.character(variable) || length(variable) != 1) {
    stop("variable must be a single character string")
  }

  # Validate logical parameters
  if (!is.logical(.lazy) || length(.lazy) != 1) {
    stop(".lazy must be a single logical value")
  }
  if (!is.logical(.number) || length(.number) != 1) {
    stop(".number must be a single logical value")
  }
  if (!is.logical(.trim) || length(.trim) != 1) {
    stop(".trim must be a single logical value")
  }

  # Validate debounce parameter
  if (!is.null(.debounce)) {
    if (!is.numeric(.debounce) || length(.debounce) != 1) {
      stop(".debounce must be NULL or a single numeric value (milliseconds)")
    }
    if (.debounce < 0) {
      stop(".debounce must be >= 0 (milliseconds)")
    }
    if (.debounce != as.integer(.debounce)) {
      warning(".debounce should be an integer (milliseconds); rounding to nearest integer")
      .debounce <- round(.debounce)
    }
  }

  # Build modifier string in Alpine.js order: .lazy -> .debounce.NNN -> .number -> .trim
  modifiers <- character(0)

  if (.lazy) {
    modifiers <- c(modifiers, "lazy")
  }

  if (!is.null(.debounce)) {
    modifiers <- c(modifiers, "debounce", as.character(as.integer(.debounce)))
  }

  if (.number) {
    modifiers <- c(modifiers, "number")
  }

  if (.trim) {
    modifiers <- c(modifiers, "trim")
  }

  # Build the attribute name with modifiers
  # Alpine.js syntax: x-model.modifier1.modifier2="variable"
  if (length(modifiers) > 0) {
    attr_name <- paste0("x-model.", paste(modifiers, collapse = "."))
  } else {
    attr_name <- "x-model"
  }

  # Append the x-model attribute using do.call for special character names
  tag <- do.call(
    htmltools::tagAppendAttributes,
    c(
      list(tag, .cssSelector = .cssSelector),
      stats::setNames(list(variable), attr_name)
    )
  )

  # Ensure Alpine.js dependency is attached
  tag <- ensure_alpine_dependency(tag)
  
  # Assign alpine_component class for S3 method dispatch
  tag <- assign_alpine_class(tag)
  
  return(tag)
}

#' Set text content from a JavaScript expression
#'
#' Creates an `x-text` attribute that sets an element's text content to the result of a
#' JavaScript expression. Alpine.js converts the expression result to a string and
#' updates the text whenever state changes.
#'
#' @param tag An htmltools tag object.
#' @param expression Character string containing a JavaScript expression that
#'   evaluates to a value to display as text. Can reference state variables
#'   from `alpine_data()` and use computed properties or method calls.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-text` attribute appended.
#'
#' @details
#' Sets text content (safe from HTML injection).
#' Expression results are converted to strings.
#'
#' @examples
#' \donttest{
#' htmltools::tags$div() |>
#'   alpine_text("itemCount")
#' }
#'
#' @family Core Directives
#' @export
alpine_text <- function(tag, expression, .cssSelector = NULL) {
  # Validate input
  if (!is.character(expression) || length(expression) != 1) {
    stop("expression must be a single character string")
  }

  # Append the x-text attribute
  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-text` = expression,
    .cssSelector = .cssSelector
  )

  # Ensure Alpine.js dependency is attached
  tag <- ensure_alpine_dependency(tag)
  
  # Assign alpine_component class for S3 method dispatch
  tag <- assign_alpine_class(tag)
  
  return(tag)
}

#' Repeat an element for each array or object item
#'
#' Creates an Alpine.js loop using `<template>` with `x-for` and optional `x-key` attributes.
#' For each item, Alpine.js renders a copy of the template element using the iteration variables.
#'
#' @param inner_tag An htmltools tag to repeat. This tag will be the template
#'   for each iteration.
#' @param expression Character string containing the iteration expression
#'   (e.g., `"item in items"`, `"(value, key) in object"`, `"(item, index) in array"`).
#'   The variable(s) defined here are available inside the repeated element.
#' @param key Optional character string specifying a unique key for each item.
#'   Example: `"item.id"`. Keys help Alpine maintain element identity when
#'   the list changes, improving performance and state preservation.
#'   If `NULL`, no `x-key` attribute is added.
#'
#' @return An htmltools `<template>` tag with `x-for` and optionally `x-key`
#'   attributes applied. Automatically includes the Alpine.js dependency.
#'   The template can be used within an `alpine_data()` component.
#'
#' @details
#' **Template Syntax:** Alpine uses `<template>` tags to wrap looped content.
#' The browser doesn't render the `<template>` tag itself, only the repeated
#' content inside it.
#'
#' **Iteration Variables:** The variable(s) in the expression are available
#' in the scope of nested elements, but not outside the template.
#'
#' **Keys:** Provide a `key` whenever your list items have a unique identifier
#' (e.g., database ID, UUID). This allows Alpine to:
#' - Match DOM elements to data items when the list reorders
#' - Preserve component state and input values during updates
#' - Improve performance by avoiding full re-renders
#'
#' **Common Patterns:**
#' ```r
#' # Array iteration
#' alpine_for(alpine_tag("div", alpine_tag("p") |> alpine_text("item.name")), "item in items", key = "item.id")
#'
#' # Array with index
#' alpine_for(alpine_tag("span") |> alpine_text("index"), "(item, index) in items", key = "item.id")
#'
#' # Object iteration
#' alpine_for(alpine_tag("div", alpine_tag("span") |> alpine_text("value")), "(value, key) in config")
#' ```
#'
#' @examples
#' \donttest{
#' alpine_for(
#'   htmltools::tags$div(htmltools::tags$span() |> alpine_text("item.name")),
#'   "item in items",
#'   key = "item.id"
#' )
#' }
#'
#' @importFrom htmltools tags
#' @family Core Directives
#' @export
alpine_for <- function(inner_tag, expression, key = NULL) {
  # Validate inputs
  if (!is.character(expression) || length(expression) != 1) {
    stop("expression must be a single character string (e.g., 'item in items')")
  }
  if (!is.null(key) && (!is.character(key) || length(key) != 1)) {
    stop("key must be NULL or a single character string (e.g., 'item.id')")
  }

  # Create template tag with inner content
  template <- htmltools::tags$template(inner_tag)
  
  # Append x-for attribute
  if (!is.null(key)) {
    template <- htmltools::tagAppendAttributes(
      template,
      `x-for` = expression,
      `x-key` = key
    )
  } else {
    template <- htmltools::tagAppendAttributes(
      template,
      `x-for` = expression
    )
  }

  # Ensure Alpine.js dependency is attached  
  template <- ensure_alpine_dependency(template)
  
  # Assign alpine_component class for S3 method dispatch
  template <- assign_alpine_class(template)
  
  return(template)
}

#' Execute a JavaScript expression on component initialization
#'
#' Creates an `x-init` attribute containing JavaScript code that Alpine.js executes
#' after the component's reactive state is initialized. Useful for loading data,
#' setting up event listeners, or running one-time setup code.
#'
#' @param tag An htmltools tag object.
#' @param expression Character string containing JavaScript code to execute.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-init` attribute appended.
#'
#' @details
#' Expression runs after `x-data` initialization.
#'
#' @examples
#' \donttest{
#' htmltools::tags$div() |>
#'   alpine_init("loadData()") |>
#'   alpine_data(data = list())
#' }
#'
#' @family Core Directives
#' @export
alpine_init <- function(tag, expression, .cssSelector = NULL) {
  # Validate input
  if (!is.character(expression) || length(expression) != 1) {
    stop("expression must be a single character string")
  }

  # Append the x-init attribute
  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-init` = expression,
    .cssSelector = .cssSelector
  )

  # Ensure Alpine.js dependency is attached
  tag <- ensure_alpine_dependency(tag)
  
  # Assign alpine_component class for S3 method dispatch
  tag <- assign_alpine_class(tag)
  
  return(tag)
}

#' Initialize reactive state with raw JavaScript
#'
#' Creates an `x-data` attribute directly from a JavaScript expression string,
#' without automatic serialization. Use when you need complex objects with methods,
#' references to global objects, or inline JavaScript patterns that R serialization
#' can't handle.
#'
#' @param tag An htmltools tag object.
#' @param js_string Character string containing a JavaScript expression.
#'   Can be:
#'   - A reference to a global object: `"window.myApp"` or `"globalState"`
#'   - An inline object literal: `"{count: 0, name: 'test', toggle: () => ...}"`
#'   - Any valid JavaScript expression that evaluates to an object
#'
#' @return The modified tag with `x-data` attribute.
#'
#' @details
#' Use for complex objects, global references, or inline methods.
#' `alpine_data_js()` takes raw JavaScript as-is. This is useful when:
#' - The object has methods (functions) that shouldn't be stringified
#' - You're using a global object defined elsewhere in your app
#' - You need templates, computed properties, or other JS patterns
#'
#' **Escaping:** The JS string is NOT escaped or validated. Ensure it's
#' valid JavaScript and properly quoted for HTML attributes.
#'
#' @examples
#' \donttest{
#' # Reference a global object
#' htmltools::tags$div(
#'   htmltools::tags$button("Reset") |> alpine_on("click", "app.reset()")
#' ) |>
#'   alpine_data_js("window.app")
#'
#' # Inline object with methods
#' htmltools::tags$div(
#'   htmltools::tags$button("Toggle") |> alpine_on("click", "toggle()"),
#'   htmltools::tags$div("Open") |> alpine_show("isOpen")
#' ) |>
#'   alpine_data_js("{
#'     isOpen: false,
#'     toggle() { this.isOpen = !this.isOpen }
#'   }")
#'
#' # Combine with tags$script for definition
#' htmltools::tags$div(
#'   htmltools::tags$script("window.calculator = {
#'     result: 0,
#'     add(n) { this.result += n },
#'     clear() { this.result = 0 }
#'   }"),
#'   htmltools::tags$button("Add 5") |> alpine_on("click", "add(5)"),
#'   htmltools::tags$span() |> alpine_text("result")
#' ) |>
#'   alpine_data_js("window.calculator")
#' }
#'
#' @family Core Directives
#' @export
alpine_data_js <- function(tag, js_string) {
  # Validate input
  if (!is.character(js_string) || length(js_string) != 1) {
    stop("js_string must be a single character string containing valid JavaScript")
  }

  # Append the x-data attribute with raw JavaScript
  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-data` = js_string
  )

  # Ensure Alpine.js dependency is attached
  tag <- ensure_alpine_dependency(tag)
  
  # Assign alpine_component class for S3 method dispatch
  tag <- assign_alpine_class(tag)
  
  return(tag)
}

#' Set inner HTML from a JavaScript expression
#'
#' Creates an `x-html` attribute that sets an element's innerHTML to the result of
#' a JavaScript expression. Unlike `alpine_text()`, the result is rendered as HTML
#' rather than plain text.
#'
#' **WARNING:** `alpine_html()` is vulnerable to XSS (cross-site scripting) when
#' used with user-provided content. Only use with trusted, controlled content.
#' Alpine.js itself warns: "Only use on trusted content and never on user-provided content."
#'
#' @param tag An htmltools tag object.
#' @param expression Character string containing a JavaScript expression.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-html` attribute appended.
#'
#' @details
#' **Security opt-in required.** Before calling `alpine_html()`, you must either:
#' - Set `options(alpinejs.unsafe = TRUE)` in your R session, or
#' - Set the environment variable `R_ALPINEJS_UNSAFE=1`
#'
#' This opt-in is required because `x-html` sets innerHTML directly, which can
#' allow arbitrary script execution if the expression value is user-controlled.
#'
#' Use `alpine_text()` instead wherever plain text rendering is sufficient.
#'
#' @examples
#' \donttest{
#' options(alpinejs.unsafe = TRUE)
#' htmltools::tags$div() |>
#'   alpine_html("formattedContent")
#' }
#'
#' @family Core Directives
#' @export
alpine_html <- function(tag, expression, .cssSelector = NULL) {
  # Require explicit opt-in due to XSS risk
  unsafe <- isTRUE(getOption("alpinejs.unsafe")) ||
            identical(Sys.getenv("R_ALPINEJS_UNSAFE"), "1")
  if (!unsafe) {
    stop(
      "alpine_html() sets innerHTML and is vulnerable to XSS when used with user-provided content.\n",
      "To enable, set options(alpinejs.unsafe = TRUE) or R_ALPINEJS_UNSAFE=1 in your environment.\n",
      "Only use with trusted content. See https://alpinejs.dev/directives/html"
    )
  }

  if (!is.character(expression) || length(expression) != 1) {
    stop("expression must be a single character string")
  }

  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-html` = expression,
    .cssSelector = .cssSelector
  )

  tag <- ensure_alpine_dependency(tag)
  tag <- assign_alpine_class(tag)
  return(tag)
}

#' Re-evaluate an expression when its dependencies change
#'
#' Creates an `x-effect` attribute containing a JavaScript expression that Alpine.js
#' re-evaluates automatically whenever any reactive data it references changes.
#' Similar to a watcher, but without specifying which properties to watch.
#'
#' @param tag An htmltools tag object.
#' @param expression Character string containing a JavaScript expression.
#'   Alpine.js tracks all reactive data accessed during evaluation and
#'   re-runs the expression when any of those values change.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-effect` attribute appended.
#'
#' @details
#' Useful for side effects that depend on reactive state, such as logging,
#' analytics, or syncing to external APIs. Alpine.js runs the expression once
#' on initialization and again whenever any referenced data changes.
#'
#' For watching a specific property, use `alpine_watch()` via `alpine_init()`.
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$input(type = "text") |> alpine_model("query")
#' ) |>
#'   alpine_effect("console.log('Query changed:', query)") |>
#'   alpine_data(query = "")
#' }
#'
#' @family Core Directives
#' @export
alpine_effect <- function(tag, expression, .cssSelector = NULL) {
  if (!is.character(expression) || length(expression) != 1) {
    stop("expression must be a single character string")
  }

  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-effect` = expression,
    .cssSelector = .cssSelector
  )

  tag <- ensure_alpine_dependency(tag)
  tag <- assign_alpine_class(tag)
  return(tag)
}

#' Label an element for direct DOM access via $refs
#'
#' Creates an `x-ref` attribute that assigns a reference name to an element.
#' The element can then be accessed within the component as `$refs.name`,
#' providing direct DOM access without needing `getElementById` or `querySelector`.
#'
#' @param tag An htmltools tag object.
#' @param ref_name Character string specifying the reference name. Must be a
#'   valid JavaScript identifier (letters, digits, underscores; no spaces).
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-ref` attribute appended.
#'
#' @details
#' Access the element in expressions using `$refs.name`. For example, after
#' `alpine_ref(input_tag, "searchBox")`, you can use `$refs.searchBox.value`
#' or call DOM methods like `$refs.searchBox.focus()`.
#'
#' Use `alpine_refs()` to generate `$refs.name` expressions for use in
#' event handlers and other directives.
#'
#' @examples
#' \donttest{
#' htmltools::tagList(
#'   htmltools::tags$input(type = "text") |> alpine_ref("content"),
#'   htmltools::tags$button("Copy") |>
#'     alpine_on("click", "navigator.clipboard.writeText($refs.content.value)")
#' ) |>
#'   (\(x) htmltools::tags$div(x))() |>
#'   alpine_data(x = "")
#' }
#'
#' @family Core Directives
#' @export
alpine_ref <- function(tag, ref_name, .cssSelector = NULL) {
  if (!is.character(ref_name) || length(ref_name) != 1) {
    stop("ref_name must be a single character string (a valid JavaScript identifier)")
  }

  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-ref` = ref_name,
    .cssSelector = .cssSelector
  )

  tag <- ensure_alpine_dependency(tag)
  tag <- assign_alpine_class(tag)
  return(tag)
}

#' Hide an element until Alpine.js has initialized
#'
#' Adds the `x-cloak` attribute, which hides the element until Alpine.js has
#' finished initializing. This prevents the "flash of unrendered content" (FOUC)
#' where unprocessed Alpine template syntax briefly appears on screen.
#'
#' The required `[x-cloak] { display: none !important; }` CSS rule is automatically
#' injected as an HTML dependency.
#'
#' @param tag An htmltools tag object.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-cloak` attribute and the required CSS dependency.
#'
#' @details
#' Alpine.js removes `x-cloak` from all elements once it has initialized, and
#' the injected CSS rule causes cloaked elements to be hidden before that point.
#'
#' Apply `x-cloak` to elements that:
#' - Contain reactive data that would appear raw before Alpine loads
#' - Are controlled by `x-show` and should not briefly flash visible
#' - Display template values via `x-text` or `x-html`
#'
#' @examples
#' \donttest{
#' htmltools::tags$div() |>
#'   alpine_text("message") |>
#'   alpine_cloak() |>
#'   alpine_data(message = "Hello")
#' }
#'
#' @family Core Directives
#' @export
alpine_cloak <- function(tag, .cssSelector = NULL) {
  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-cloak` = "",
    .cssSelector = .cssSelector
  )

  # Inject the CSS rule required for x-cloak to work
  cloak_dep <- htmltools::htmlDependency(
    name    = "alpine-cloak-css",
    version = "1.0",
    src     = c(href = ""),
    head    = "<style>[x-cloak] { display: none !important; }</style>"
  )
  tag <- htmltools::attachDependencies(tag, cloak_dep, append = TRUE)

  tag <- ensure_alpine_dependency(tag)
  tag <- assign_alpine_class(tag)
  return(tag)
}

#' Prevent Alpine.js from initializing a block of HTML
#'
#' Adds the `x-ignore` attribute, which tells Alpine.js to skip the element
#' and all of its children during initialization. Useful for embedding
#' third-party widgets or code that should not be processed by Alpine.
#'
#' @param tag An htmltools tag object.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-ignore` attribute appended.
#'
#' @examples
#' \donttest{
#' htmltools::tags$div(
#'   htmltools::tags$p("This paragraph uses x-text.") |> alpine_text("msg"),
#'   htmltools::tags$div("Third-party widget -- Alpine will leave this alone.") |>
#'     alpine_ignore()
#' ) |>
#'   alpine_data(msg = "Hello")
#' }
#'
#' @family Core Directives
#' @export
alpine_ignore <- function(tag, .cssSelector = NULL) {
  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-ignore` = "",
    .cssSelector = .cssSelector
  )

  tag <- ensure_alpine_dependency(tag)
  tag <- assign_alpine_class(tag)
  return(tag)
}

#' Conditionally add or remove an element from the DOM
#'
#' Creates a `<template x-if="condition">` wrapper that adds or completely removes
#' its child element based on a JavaScript condition. Unlike `alpine_show()` which
#' uses CSS `display: none`, `alpine_if()` fully removes the element from the DOM
#' when the condition is false.
#'
#' @param inner_tag An htmltools tag to conditionally render. This becomes the
#'   single root element inside the template.
#' @param condition Character string containing a JavaScript boolean expression.
#'
#' @return An htmltools `<template>` tag with `x-if` attribute. Automatically
#'   includes the Alpine.js dependency.
#'
#' @details
#' **`x-if` vs `x-show`:**
#' - `alpine_if()` removes the element from the DOM when false (heavier for toggling,
#'   better for elements that are rarely shown or have expensive initialization).
#' - `alpine_show()` keeps the element in the DOM and toggles `display: none`
#'   (lighter for frequent toggling, element state is preserved).
#'
#' **Template restriction:** `x-if` must be used on a `<template>` tag, and the
#' template must contain exactly one root element. `alpine_if()` handles the
#' wrapping automatically.
#'
#' **Transition:** `x-if` does not support `x-transition`. Use `alpine_show()` with
#' `alpine_transition()` if you need enter/leave animations.
#'
#' @examples
#' \donttest{
#' alpine_if(
#'   htmltools::tags$div("Only rendered when open is TRUE"),
#'   "open"
#' )
#' }
#'
#' @importFrom htmltools tags
#' @family Core Directives
#' @export
alpine_if <- function(inner_tag, condition) {
  if (!is.character(condition) || length(condition) != 1) {
    stop("condition must be a single character string")
  }

  template <- htmltools::tags$template(inner_tag)
  template <- htmltools::tagAppendAttributes(template, `x-if` = condition)

  template <- ensure_alpine_dependency(template)
  template <- assign_alpine_class(template)
  return(template)
}

#' Expose a data property for binding by a parent x-model
#'
#' Creates an `x-modelable` attribute that makes a component's data property
#' available as the bind target when a parent component uses `x-model` on this element.
#' This is the mechanism for building custom form inputs that integrate with Alpine's
#' two-way binding system.
#'
#' @param tag An htmltools tag object.
#' @param property Character string specifying the internal data property to expose.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-modelable` attribute appended.
#'
#' @details
#' When a parent element uses `x-model="someVar"` on a child component that has
#' `x-modelable="internalProp"`, Alpine.js synchronizes `someVar` in the parent
#' with `internalProp` in the child component.
#'
#' This pattern is useful for encapsulating form input components (sliders, pickers,
#' rich text editors) that manage their own internal state but need to integrate
#' with parent form data.
#'
#' @examples
#' \donttest{
#' # Child component exposing its value
#' htmltools::tags$div(
#'   htmltools::tags$input(type = "range", min = "0", max = "100") |>
#'     alpine_model("value")
#' ) |>
#'   alpine_modelable("value") |>
#'   alpine_data(value = 50)
#' }
#'
#' @family Core Directives
#' @export
alpine_modelable <- function(tag, property, .cssSelector = NULL) {
  if (!is.character(property) || length(property) != 1) {
    stop("property must be a single character string")
  }

  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-modelable` = property,
    .cssSelector = .cssSelector
  )

  tag <- ensure_alpine_dependency(tag)
  tag <- assign_alpine_class(tag)
  return(tag)
}

#' Apply enter/leave transitions to an element
#'
#' Adds `x-transition` attributes to animate an element when it is shown or
#' hidden via `x-show`. Two modes are supported: the transition helper (using
#' modifiers for common fade/scale effects) and CSS class mode (for full control
#' using Tailwind or custom CSS classes).
#'
#' @param tag An htmltools tag object that also has `x-show` (via `alpine_show()`).
#' @param .duration Numeric. Transition duration in milliseconds (helper mode only).
#' @param .delay Numeric. Transition delay in milliseconds (helper mode only).
#' @param .opacity Logical. If `TRUE`, apply opacity transition only (no scale).
#'   Helper mode only.
#' @param .scale Logical. If `TRUE`, include a scale transition. Helper mode only.
#' @param .scale_value Numeric 0-100. Scale percentage (e.g., `80` scales to 80%).
#'   Only used when `.scale = TRUE`. Helper mode only.
#' @param .origin Character. Scale origin: `"top"`, `"bottom"`, `"left"`, `"right"`,
#'   or space-separated combinations like `"top right"`. Only used with `.scale = TRUE`.
#' @param .enter Character. CSS classes applied for the entire enter phase.
#' @param .enter_start Character. CSS classes for the start of enter (before element appears).
#' @param .enter_end Character. CSS classes for the end of enter (element fully visible).
#' @param .leave Character. CSS classes applied for the entire leave phase.
#' @param .leave_start Character. CSS classes for the start of leave.
#' @param .leave_end Character. CSS classes for the end of leave (element hidden).
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-transition` attribute(s) appended.
#'
#' @details
#' **Helper mode** (default, using modifiers):
#' Applies a built-in fade + scale effect. Add modifiers to customize:
#' - `.opacity = TRUE`: fade only (no scale)
#' - `.scale = TRUE`: scale only (with optional `.scale_value` and `.origin`)
#' - `.duration = 500`: 500ms transition
#' - `.delay = 50`: 50ms delay
#'
#' Helper mode produces a single `x-transition[.modifier.modifier...]=""` attribute.
#'
#' **CSS class mode** (when any of `.enter`, `.enter_start`, `.enter_end`,
#' `.leave`, `.leave_start`, `.leave_end` are provided):
#' Attaches separate `x-transition:phase="classes"` attributes for each phase.
#' This is used with utility CSS frameworks like Tailwind CSS.
#'
#' You can mix both modes: use CSS class mode for enter/leave phases alongside
#' helper mode modifiers where appropriate.
#'
#' @examples
#' \donttest{
#' # Helper mode: fade + scale, 300ms
#' htmltools::tags$div("Hello") |>
#'   alpine_show("open") |>
#'   alpine_transition(.duration = 300)
#'
#' # Helper mode: opacity only
#' htmltools::tags$div("Hello") |>
#'   alpine_show("open") |>
#'   alpine_transition(.opacity = TRUE)
#'
#' # CSS class mode (Tailwind)
#' htmltools::tags$div("Hello") |>
#'   alpine_show("open") |>
#'   alpine_transition(
#'     .enter       = "transition ease-out duration-300",
#'     .enter_start = "opacity-0 scale-90",
#'     .enter_end   = "opacity-100 scale-100",
#'     .leave       = "transition ease-in duration-300",
#'     .leave_start = "opacity-100 scale-100",
#'     .leave_end   = "opacity-0 scale-90"
#'   )
#' }
#'
#' @family Core Directives
#' @export
alpine_transition <- function(
  tag,
  .duration    = NULL,
  .delay       = NULL,
  .opacity     = FALSE,
  .scale       = FALSE,
  .scale_value = NULL,
  .origin      = NULL,
  .enter       = NULL,
  .enter_start = NULL,
  .enter_end   = NULL,
  .leave       = NULL,
  .leave_start = NULL,
  .leave_end   = NULL,
  .cssSelector = NULL
) {
  css_class_mode <- !is.null(.enter) || !is.null(.enter_start) || !is.null(.enter_end) ||
                    !is.null(.leave)  || !is.null(.leave_start) || !is.null(.leave_end)

  if (css_class_mode) {
    # CSS class mode: attach separate x-transition:phase attributes
    css_phases <- list(
      "x-transition:enter"       = .enter,
      "x-transition:enter-start" = .enter_start,
      "x-transition:enter-end"   = .enter_end,
      "x-transition:leave"       = .leave,
      "x-transition:leave-start" = .leave_start,
      "x-transition:leave-end"   = .leave_end
    )
    for (phase_name in names(css_phases)) {
      val <- css_phases[[phase_name]]
      if (!is.null(val)) {
        tag <- do.call(
          alpine_append_attributes,
          c(list(tag), stats::setNames(list(val), phase_name))
        )
      }
    }
  }

  # Helper mode: build x-transition[.modifier...] attribute (always included
  # unless css_class_mode is TRUE and no helper options were given)
  has_helper_opts <- .opacity || .scale || !is.null(.duration) || !is.null(.delay)

  if (!css_class_mode || has_helper_opts) {
    modifiers <- character(0)

    if (.opacity && !.scale) {
      modifiers <- c(modifiers, "opacity")
    }

    if (.scale) {
      if (!is.null(.scale_value)) {
        modifiers <- c(modifiers, "scale", as.character(as.integer(.scale_value)))
      } else {
        modifiers <- c(modifiers, "scale")
      }
      if (!is.null(.origin)) {
        origin_parts <- strsplit(trimws(.origin), "\\s+")[[1]]
        modifiers <- c(modifiers, "origin", origin_parts)
      }
    }

    if (!is.null(.duration)) {
      modifiers <- c(modifiers, "duration", paste0(as.integer(.duration), "ms"))
    }

    if (!is.null(.delay)) {
      modifiers <- c(modifiers, "delay", paste0(as.integer(.delay), "ms"))
    }

    attr_name <- if (length(modifiers) > 0) {
      paste0("x-transition.", paste(modifiers, collapse = "."))
    } else {
      "x-transition"
    }

    tag <- do.call(
      alpine_append_attributes,
      c(list(tag), stats::setNames(list(""), attr_name))
    )
  }

  tag <- ensure_alpine_dependency(tag)
  tag <- assign_alpine_class(tag)
  return(tag)
}

#' Teleport an element to another part of the DOM
#'
#' Creates a `<template x-teleport="selector">` wrapper that moves the element's
#' rendered output to a different DOM node (matched by the CSS selector) rather
#' than rendering it in place. This is useful for modals, tooltips, and overlays
#' that need to be rendered outside their parent's stacking context or overflow.
#'
#' @param tag An htmltools tag to teleport. This becomes the content of the template.
#' @param selector Character string CSS selector identifying the destination element
#'   (e.g., `"body"`, `"#modal-container"`, `".overlay-root"`).
#'
#' @return An htmltools `<template>` tag with `x-teleport` attribute. The content
#'   will be rendered at the selector's target in the browser DOM.
#'
#' @details
#' The element is placed in the document where `alpine_teleport()` is called
#' (as a `<template>` tag), but its rendered content appears at the target
#' selector in the browser. Alpine.js reactive data from the origin component
#' is still accessible in the teleported content.
#'
#' @examples
#' \donttest{
#' # Teleport a modal to the body element
#' alpine_teleport(
#'   htmltools::tags$div(class = "modal",
#'     htmltools::tags$p("Modal content")
#'   ),
#'   selector = "body"
#' )
#' }
#'
#' @importFrom htmltools tags
#' @family Core Directives
#' @export
alpine_teleport <- function(tag, selector) {
  if (!is.character(selector) || length(selector) != 1) {
    stop("selector must be a single character string (e.g., 'body', '#modal-root')")
  }

  template <- htmltools::tags$template(tag)
  template <- htmltools::tagAppendAttributes(template, `x-teleport` = selector)

  template <- ensure_alpine_dependency(template)
  template <- assign_alpine_class(template)
  return(template)
}

#' Define a unique ID scope for a group of related elements
#'
#' Creates an `x-id` attribute that establishes a scope for generating unique IDs
#' with the `$id()` magic property. This allows multiple instances of the same
#' component to have unique, non-conflicting IDs for accessibility (e.g., linking
#' labels to inputs).
#'
#' @param tag An htmltools tag object.
#' @param ... One or more character strings naming the ID scopes. Each name is
#'   used as a prefix when generating IDs with `$id('name')`.
#' @param .cssSelector Optional CSS selector for targeting a nested element.
#'
#' @return The modified tag with `x-id` attribute appended.
#'
#' @details
#' Within the component, use `$id('scope-name')` to generate a unique ID for
#' that scope. Alpine.js ensures that each component instance gets a different
#' numeric suffix (e.g., `tooltip-button-1`, `tooltip-button-2`), enabling safe
#' re-use of components without ID collisions.
#'
#' Use `alpine_id_ref()` to generate `$id('name')` expression strings for
#' attributes like `for` and `id`.
#'
#' @examples
#' \donttest{
#' # Component with two related ID scopes
#' htmltools::tags$div(
#'   htmltools::tags$label() |>
#'     alpine_bind("for", "$id('search-input')") |>
#'     alpine_text("'Search'"),
#'   htmltools::tags$input(type = "text") |>
#'     alpine_bind("id", "$id('search-input')")
#' ) |>
#'   alpine_id("search-input") |>
#'   alpine_data(x = "")
#' }
#'
#' @family Core Directives
#' @export
alpine_id <- function(tag, ..., .cssSelector = NULL) {
  scope_names <- c(...)
  if (!is.character(scope_names) || length(scope_names) == 0) {
    stop("Provide at least one scope name as a character string.")
  }

  # Build a JavaScript array string: ['name1', 'name2']
  js_array <- paste0("['", paste(scope_names, collapse = "', '"), "']")

  tag <- htmltools::tagAppendAttributes(
    tag,
    `x-id` = js_array,
    .cssSelector = .cssSelector
  )

  tag <- ensure_alpine_dependency(tag)
  tag <- assign_alpine_class(tag)
  return(tag)
}
