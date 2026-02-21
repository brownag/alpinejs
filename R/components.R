#' Accordion (Collapsible Panels) Component
#'
#' Creates a collapsible accordion with multiple panels that users can expand and
#' collapse individually. Alpine.js manages the panel state and visibility using
#' reactive directives.
#'
#' @param items A list of items, each with `title` and `content` fields.
#'   Example: `list(list(title = "Item 1", content = "Body 1"), ...)`
#'   Alternatively, can use named lists: `list(title1 = "Body 1", title2 = "Body 2")`
#' @param class Optional CSS class(es) to apply to the accordion container.
#'   Useful for styling (e.g., "accordion accordion-flush").
#' @param title_class Optional CSS class(es) for accordion item headers.
#' @param content_class Optional CSS class(es) for accordion item bodies.
#'
#' @return An htmltools tag object representing the accordion. Pipe-compatible.
#'
#' @details
#' The accordion uses Alpine.js reactive state to track which panels are open.
#'
#' @examples
#' \donttest{
#' alpine_accordion(
#'   items = list(
#'     list(title = "Section 1", content = "Content 1"),
#'     list(title = "Section 2", content = "Content 2"),
#'     list(title = "Section 3", content = "Content 3")
#'   )
#' )
#' }
#'
#' @family UI Components
#' @export
alpine_accordion <- function(items, class = NULL, title_class = NULL,
                             content_class = NULL) {
  # Validate items structure
  if (!is.list(items) || length(items) == 0) {
    stop("items must be a non-empty list of lists with 'title' and 'content'")
  }

  # Convert items to list of lists if necessary
  if (!is.null(names(items)) && all(c("title", "content") %in% names(items[[1]]))) {
    # items is already list of lists with title/content
    items_list <- items
  } else if (!is.null(names(items)) && length(items[[1]]) == 1) {
    # items might be named list; convert
    items_list <- lapply(seq_along(items), function(i) {
      list(title = names(items)[[i]], content = items[[i]])
    })
  } else {
    items_list <- items
  }

  # Verify all items have title and content
  for (i in seq_along(items_list)) {
    if (!is.list(items_list[[i]]) || !all(c("title", "content") %in% names(items_list[[i]]))) {
      stop("Each item must be a list with 'title' and 'content' fields")
    }
  }

  # Build the accordion structure
  n_items <- length(items_list)
  collapsed_initial <- rep(TRUE, n_items)  # all panels collapsed initially

  # Build panel divs
  panels <- lapply(seq_along(items_list), function(i) {
    item <- items_list[[i]]
    title_text <- item$title
    content_text <- item$content

    htmltools::tags$div(
      class = c("accordion-item", title_class),
      htmltools::tags$button(
        title_text,
        class = "accordion-header"
      ) |>
        alpine_on("click", sprintf("togglePanel(%d)", i - 1)),
      htmltools::tags$div(
        content_text,
        class = c("accordion-content", content_class)
      ) |>
        alpine_show(sprintf("!collapsed[%d]", i - 1))
    )
  })

  # Build the main accordion container
  accordion_div <- do.call(
    htmltools::tags$div,
    c(list(class = class), panels)
  )

  # Add Alpine state and methods
  accordion_div |>
    alpine_data(
      collapsed = collapsed_initial,
      togglePanel = htmlwidgets::JS(sprintf(
        "function(index) { this.$nextTick(() => { this.collapsed[index] = !this.collapsed[index]; }); }"
      ))
    )
}

#' Alpine.js Modal Component
#'
#' Creates a modal dialog component with an optional trigger button, title,
#' and content. The modal is toggled via a boolean state variable.
#'
#' @param trigger_label Character string for the trigger button label
#'   (e.g., "Open Modal", "Edit Item"). If `NULL`, no trigger button is
#'   rendered (useful for programmatic control).
#' @param title Character string or htmltools tag for the modal title/header.
#' @param content Character string or htmltools tag for the modal body content.
#' @param close_label Character string for the close button label. Defaults to "Close".
#' @param class Optional CSS class(es) for the modal container.
#' @param backdrop Logical or character. If `TRUE` (default), renders a
#'   semi-transparent backdrop overlay that closes the modal on click.
#'   If a character string, treated as the CSS class for the backdrop.
#'
#' @return An htmltools tag object representing the modal. Pipe-compatible.
#'
#' @details
#' The modal consists of:
#' - Optional trigger button
#' - Backdrop overlay (optional)
#' - Modal dialog with header, body content, and footer with close button
#'
#' **CSS Styling:** The component provides minimal default styling.
#' Target these classes for styling:
#' - `.modal-backdrop` - overlay backdrop
#' - `.modal-dialog` - modal container
#' - `.modal-header` - title area
#' - `.modal-body` - content area
#' - `.modal-footer` - button footer
#'
#' **State:**
#' ```javascript
#' { modalOpen: false }
#' ```
#'
#' **Customization:** To customize appearance, add CSS rules or pass
#' styled elements as `title` and `content`.
#'
#' @examples
#' \donttest{
#' alpine_modal(
#'   trigger_label = "Open Modal",
#'   title = "Welcome",
#'   content = "Modal content"
#' )
#' }
#'
#' @family UI Components
#' @export
alpine_modal <- function(trigger_label = "Open",
                         title = "Modal Title",
                         content = "Modal content",
                         close_label = "Close",
                         class = NULL,
                         backdrop = TRUE) {
  # Build trigger button
  trigger_btn <- if (!is.null(trigger_label)) {
    htmltools::tags$button(
      trigger_label,
      class = "btn btn-primary"
    ) |>
      alpine_on("click", "modalOpen = true")
  } else {
    NULL
  }

  # Build backdrop
  backdrop_div <- if (isTRUE(backdrop) || is.character(backdrop)) {
    backdrop_class <- if (is.character(backdrop)) backdrop else "modal-backdrop"
    htmltools::tags$div(
      class = backdrop_class,
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 999;"
    ) |>
      alpine_show("modalOpen") |>
      alpine_on("click", "modalOpen = false")
  } else {
    NULL
  }

  # Build modal dialog
  modal_dialog <- htmltools::tags$div(
    class = c("modal-dialog", class),
    style = "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); background: white; padding: 20px; border-radius: 8px; z-index: 1000; min-width: 300px;",
    htmltools::tags$div(
      class = "modal-header",
      htmltools::tags$h2(title, style = "margin: 0 0 15px 0;"),
      htmltools::tags$button(
        "x",
        style = "position: absolute; top: 10px; right: 10px; border: none; background: none; font-size: 24px; cursor: pointer;"
      ) |>
        alpine_on("click", "modalOpen = false")
    ),
    htmltools::tags$div(
      class = "modal-body",
      content,
      style = "margin-bottom: 15px;"
    ),
    htmltools::tags$div(
      class = "modal-footer",
      style = "display: flex; gap: 10px; justify-content: flex-end;",
      htmltools::tags$button(
        close_label,
        class = "btn btn-secondary"
      ) |>
        alpine_on("click", "modalOpen = false")
    )
  ) |>
    alpine_show("modalOpen")

  # Combine elements
  container <- htmltools::div(
    trigger_btn,
    backdrop_div,
    modal_dialog
  )

  # Add Alpine state
  container |>
    alpine_data(modalOpen = FALSE)
}

#' Alpine.js Tabs Component
#'
#' Creates a tabbed interface component with multiple tabs and content panels.
#' Only one tab's content is visible at a time, controlled via Alpine state.
#'
#' @param tabs A list of tabs, each with `label` and `content` fields.
#'   Example: `list(list(label = "Tab 1", content = "Content 1"), ...)`
#' @param class Optional CSS class(es) for the tabs container.
#' @param tab_class Optional CSS class(es) for individual tab buttons.
#' @param content_class Optional CSS class(es) for tab content panels.
#'
#' @return An htmltools tag object representing the tabs. Pipe-compatible.
#'
#' @details
#' The tabs component renders:
#' - A tab list (buttongroup) with clickable tab labels
#' - Content panels that show/hide based on the active tab index
#' @details
#' Component maintains active tab state via index.
#' Use CSS classes to customize styling.
#'
#' @examples
#' \donttest{
#' alpine_tabs(
#'   tabs = list(
#'     list(label = "Home", content = "Home content"),
#'     list(label = "About", content = "About content"),
#'     list(label = "Contact", content = "Contact content")
#'   )
#' )
#' }
#'
#' @family UI Components
#' @export
alpine_tabs <- function(tabs, class = NULL, tab_class = NULL,
                        content_class = NULL) {
  # Validate tabs structure
  if (!is.list(tabs) || length(tabs) == 0) {
    stop("tabs must be a non-empty list of lists with 'label' and 'content'")
  }

  # Convert to list of lists if necessary
  if (!is.null(names(tabs)) && all(c("label", "content") %in% names(tabs[[1]]))) {
    tabs_list <- tabs
  } else {
    tabs_list <- tabs
  }

  # Verify all tabs have label and content
  for (i in seq_along(tabs_list)) {
    if (!is.list(tabs_list[[i]]) || !all(c("label", "content") %in% names(tabs_list[[i]]))) {
      stop("Each tab must be a list with 'label' and 'content' fields")
    }
  }

  # Build tab buttons
  tab_buttons <- lapply(seq_along(tabs_list), function(i) {
    button_label <- tabs_list[[i]]$label

    # Create button with dynamic class binding
    htmltools::tags$button(
      button_label,
      class = "tab-button"
    ) |>
      alpine_on("click", sprintf("activeTab = %d", i - 1)) |>
      alpine_bind("class", sprintf("activeTab === %d ? 'tab-button active' : 'tab-button'", i - 1))
  })

  # Build tab content panels
  tab_panels <- lapply(seq_along(tabs_list), function(i) {
    panel_content <- tabs_list[[i]]$content

    htmltools::tags$div(
      class = c("tab-panel", content_class),
      panel_content
    ) |>
      alpine_show(sprintf("activeTab === %d", i - 1))
  })

  # Combine into tabs container
  tabs_container <- htmltools::tags$div(
    class = c("tabs-container", class),
    do.call(
      htmltools::tags$div,
      c(
        list(
          class = "tabs-list",
          style = "display: flex; gap: 10px; border-bottom: 1px solid #ccc; margin-bottom: 15px;"
        ),
        tab_buttons
      )
    ),
    do.call(
      htmltools::tags$div,
      c(
        list(class = "tabs-content"),
        tab_panels
      )
    )
  )

  # Add Alpine state
  tabs_container |>
    alpine_data(activeTab = 0)
}

#' Alpine.js Form Field Component
#'
#' Creates a labeled input field with optional error message display,
#' using two-way data binding via `alpine_model()` and conditional error
#' visibility via `alpine_show()` and `alpine_text()`.
#'
#' @param label Character string or htmltools tag for the field label.
#' @param input_tag An htmltools tag (typically `input()` or `textarea()`)
#'   to use as the input element. The tag should not have `x-model` already
#'   applied; this function will add it.
#' @param model_var Character string specifying the state variable for
#'   two-way binding. Example: `"emailAddress"`, `"userAge"`.
#' @param error_var Optional character string specifying the error message
#'   variable. If `NULL` (default), no error div is rendered. If provided,
#'   the error div will show when this variable is non-empty/truthy.
#'   Example: `"emailError"`.
#' @param label_style Optional character string with inline CSS for the label.
#' @param error_style Optional character string with inline CSS for the error message.
#'   Default colors and styling are applied if not specified.
#' @param input_class Optional CSS class(es) for the input element.
#' @param container_class Optional CSS class(es) for the outer container div.
#' @param ... Additional attributes passed to the input tag (e.g., modifiers
#'   like `.lazy = TRUE` or `.debounce = 300` for `alpine_model`).
#'   Note: These are passed to `alpine_model()`, not to the tag constructor.
#'
#' @return An htmltools tag object representing the field container with
#'   label (if provided), input, and optional error div. Pipe-compatible.
#'
#' @details
#' Component contains label, input, and optional error div.
#'
#' ```r
#' htmltools::tags$div(
#'   alpine_field(
#'     label = "Email",
#'     input_tag = htmltools::tags$input(type = "email", placeholder = "name@example.com"),
#'     model_var = "email",
#'     error_var = "emailError"
#'   ),
#'   htmltools::tags$button("Submit")
#' ) |>
#'   alpine_data(
#'     email = "",
#'     emailError = "",
#'     validate = htmlwidgets::JS("() => {
#'       if (!this.email.includes('@')) {
#'         this.emailError = 'Invalid email address';
#'       } else {
#'         this.emailError = '';
#'       }
#'     }")
#'   )
#' ```
#'
#' **Styling Customization:**
#' - Use `label_style` to customize the label appearance
#' - Use `error_style` to customize error text color and styling
#' - Use `input_class` to add CSS classes to the input (e.g., Tailwind, Bootstrap)
#' - Use `container_class` to style the overall field container
#'
#' @examples
#' \donttest{
#' alpine_field(
#'   label = "Name",
#'   input_tag = htmltools::tags$input(type = "text", placeholder = "Enter name"),
#'   model_var = "userName"
#' )
#' }
#'
#' @importFrom htmltools tags tagAppendAttributes
#' @family UI Components
#' @export
alpine_field <- function(label = NULL,
                         input_tag,
                         model_var,
                         error_var = NULL,
                         label_style = NULL,
                         error_style = NULL,
                         input_class = NULL,
                         container_class = NULL,
                         ...) {
  # Validate inputs
  if (!is.character(model_var) || length(model_var) != 1) {
    stop("model_var must be a single character string")
  }
  if (!is.null(error_var) && (!is.character(error_var) || length(error_var) != 1)) {
    stop("error_var must be NULL or a single character string")
  }

  # Add alpine_model to input tag with any passed modifiers
  bound_input <- do.call(
    alpine_model,
    c(list(tag = input_tag, variable = model_var), list(...))
  )

  # Add class if provided
  if (!is.null(input_class)) {
    bound_input <- htmltools::tagAppendAttributes(bound_input, class = input_class)
  }

  # Build label if provided
  label_elem <- if (!is.null(label)) {
    htmltools::tags$label(
      label,
      style = c("display: block; margin-bottom: 4px;", label_style),
      `for` = model_var
    )
  } else {
    NULL
  }

  # Build error div if error_var provided
  error_div <- if (!is.null(error_var)) {
    htmltools::tags$div(
      style = c(
        "margin-top: 4px; color: #dc3545; font-size: 0.9em;",
        error_style
      )
    ) |>
      alpine_text(error_var) |>
      alpine_show(error_var)
  } else {
    NULL
  }

  # Combine into container
  htmltools::tags$div(
    class = container_class,
    label_elem,
    bound_input,
    error_div
  )
}

#' Alpine.js Stat Card Component
#'
#' Creates a colored stat card displaying a label and a reactive value.
#' The value is rendered via `alpine_text()` and updates reactively as the
#' underlying state changes.
#'
#' @param label Character string or htmltools tag for the label text
#'   (e.g., "Total Users", "Revenue").
#' @param value_expr Character string containing a JavaScript expression
#'   that evaluates to the displayed value. Can be a simple state variable
#'   (e.g., `"count"`) or a computed expression (e.g., `"items.length"`).
#' @param bg_color Character string with the card background color
#'   (hex, rgb, or color name). Default: `"#f5f5f5"` (light gray).
#' @param text_color Character string with the main text color (label and borders).
#'   Default: `"#333"` (dark gray).
#' @param accent_color Optional character string for accent elements
#'   (e.g., value text, borders). If `NULL`, uses the same as `text_color`.
#' @param border_style Optional character string for CSS border styling.
#'   If `NULL`, no border is used. Example: `"1px solid #ddd"`.
#'
#' @return An htmltools tag object representing the stat card.
#'   Pipe-compatible.
#'
#' @details
#' The card is a styled div with two text elements:
#' - A label line
#' - A large value line (using `x-text` for reactivity)
#'
#' **Styling:** Minimal default styling is applied. Use the color parameters
#' to customize. For more complex styling, wrap the component in a styled
#' container or use the class parameter.
#'
#' **Value Updates:** Because the value is rendered via `alpine_text()`, it
#' automatically updates whenever the underlying state variable changes.
#' This is useful for dashboard cards, metrics, and counters.
#'
#' @examples
#' \donttest{
#' alpine_stat_card(
#'   label = "Total Users",
#'   value_expr = "userCount",
#'   bg_color = "#e3f2fd"
#' ) |>
#'   alpine_data(userCount = 42)
#' }
#'
#' @importFrom htmltools tags tagAppendAttributes
#' @family UI Components
#' @export
alpine_stat_card <- function(label,
                      value_expr,
                      bg_color = "#f5f5f5",
                      text_color = "#333",
                      accent_color = NULL,
                      border_style = NULL) {
  # Default accent_color to text_color if not provided
  if (is.null(accent_color)) {
    accent_color <- text_color
  }

  # Validate inputs
  if (!is.character(label) || length(label) != 1) {
    stop("label must be a single character string")
  }
  if (!is.character(value_expr) || length(value_expr) != 1) {
    stop("value_expr must be a single character string")
  }

  # Build border CSS if provided
  border_css <- if (!is.null(border_style)) {
    paste0("border: ", border_style, ";")
  } else {
    ""
  }

  # Build the card
  htmltools::tags$div(
    class = "stat-card",
    style = paste0(
      "background-color: ", bg_color, "; ",
      "padding: 16px; ",
      "border-radius: 8px; ",
      "text-align: center; ",
      "color: ", text_color, "; ",
      border_css
    ),
    htmltools::tags$div(
      label,
      style = paste0(
        "font-size: 14px; ",
        "font-weight: 500; ",
        "margin-bottom: 8px;",
        "opacity: 0.8;"
      )
    ),
    htmltools::tags$div(
      style = paste0(
        "font-size: 28px; ",
        "font-weight: bold; ",
        "color: ", accent_color, ";"
      )
    ) |>
      alpine_text(value_expr)
  )
}

#' Alpine.js Filter/Tab Button Group Component
#'
#' Creates a group of buttons for filtering or selecting options.
#' Typically used to toggle between different views or filter states.
#' Uses `alpine_on()` for click events and `alpine_bind()` for dynamic styling
#' of the active button.
#'
#' @param choices A named character vector or list where names are button
#'   labels and values are the values to assign to the state variable.
#'   Example: `c("All" = "", "Active" = "active", "Archived" = "archived")`
#'   or `list("Option 1" = "val1", "Option 2" = "val2")`.
#' @param state_var Character string specifying the state variable that
#'   tracks the currently selected value. Example: `"selectedFilter"`.
#' @param active_style Optional character string with inline CSS for active
#'   button styling. Default applies `background-color: #007bff; color: white;`
#' @param inactive_style Optional character string with inline CSS for inactive
#'   button styling. Default applies lighter gray background.
#' @param button_class Optional CSS class(es) to apply to all buttons.
#'   Useful for framework integration (e.g., Bootstrap, Tailwind).
#' @param container_class Optional CSS class(es) for the button group
#'   container (wrapper div).
#'
#' @return An htmltools tag object (div) containing the button group,
#'   with each button bound to the specified state variable.
#'   Pipe-compatible.
#'
#' @details
#' The component generates a div with multiple buttons, each with:
#' - An `alpine_on("click", ...)` handler that sets the state variable
#' - An `alpine_bind()` that dynamically applies active/inactive styling
#'
#' **Styling Strategy:**
#' - Provide `active_style` for the selected button's appearance
#' - Provide `inactive_style` for unselected buttons
#' - Use `button_class` to add base styles (padding, border, cursor, etc.)
#' - Combine with CSS frameworks for advanced styling
#'
#' **State Tracking:** The component tracks which button is active by
#' comparing the state variable value with each button's value. The first
#' button matching `state_var === value` receives the active styling.
#'
#' @examples
#' \donttest{
#' alpine_filter_buttons(
#'   choices = c("All" = "", "Active" = "active", "Inactive" = "inactive"),
#'   state_var = "selectedStatus"
#' ) |>
#'   alpine_data(selectedStatus = "")
#' }
#'
#' @importFrom htmltools tags
#' @family UI Components
#' @export
alpine_filter_buttons <- function(choices,
                           state_var,
                           active_style = NULL,
                           inactive_style = NULL,
                           button_class = NULL,
                           container_class = NULL) {
  # Validate inputs
  if (!is.character(state_var) || length(state_var) != 1) {
    stop("state_var must be a single character string")
  }
  if (!is.list(choices) && !is.character(choices)) {
    stop("choices must be a named character vector or list")
  }

  # Convert to named character vector
  if (is.list(choices)) {
    choice_names <- names(choices) %||% seq_along(choices)
    choice_values <- as.character(unlist(choices))
  } else {
    choice_names <- names(choices) %||% as.character(choices)
    choice_values <- as.character(choices)
  }

  # Default styles
  if (is.null(active_style)) {
    active_style <- "background-color: #007bff; color: white;"
  }
  if (is.null(inactive_style)) {
    inactive_style <- "background-color: #e9ecef; color: #333;"
  }

  # Build buttons
  buttons <- Map(function(name, value) {
    htmltools::tags$button(
      name,
      class = button_class,
      style = "padding: 8px 16px; margin: 4px; border: none; border-radius: 4px; cursor: pointer; transition: background-color 0.2s;"
    ) |>
      alpine_on("click", sprintf("%s = '%s'", state_var, value)) |>
      alpine_bind(
        "style",
        sprintf(
          "%s === '%s' ? '%s' : '%s'",
          state_var, value, active_style, inactive_style
        )
      )
  }, choice_names, choice_values)

  # Combine buttons into container
  do.call(
    htmltools::tags$div,
    c(
      list(class = container_class, style = "display: flex; gap: 4px; flex-wrap: wrap;"),
      buttons
    )
  )
}

