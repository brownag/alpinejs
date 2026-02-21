#' Alpine Component Class
#'
#' The `alpine_component` S3 class is automatically assigned to all alpinejs
#' functions that return HTML elements. This enables special handling in
#' different contexts (console, vignettes, RStudio Viewer) through S3 method
#' dispatch.
#'
#' @name alpine_component
#' @docType class
#'
#' @details
#'
#' ## Automatic Assignment
#'
#' The class is automatically assigned to outputs from:
#' - `alpine_tag()` - Base HTML tag creation
#' - Modifier functions: `alpine_data()`, `alpine_on()`, `alpine_show()`,
#'   `alpine_bind()`, `alpine_model()`, `alpine_text()`, `alpine_for()`,
#'   `alpine_init()`, `alpine_data_js()`
#' - Component functions: `alpine_accordion()`, `alpine_button()`,
#'   `alpine_modal()`, `alpine_tabs()`, etc.
#' - Helper functions that return HTML
#'
#' ## S3 Methods
#'
#' Users can interact with `alpine_component` objects through these S3 methods:
#'
#' - `print(x)` - Print raw HTML to console
#' - `knit_print(x, ...)` - Render in R Markdown. Wraps HTML in interactive
#'   JavaScript widget to preserve camelCase attributes during pkgdown build.
#' - `alpine_browser(x)` - Explicitly open in RStudio Viewer for interactive preview
#'
#' ## Why This Class Exists
#'
#' The Alpine.js library requires exact attribute name casing (e.g.,
#' `x-bind:textContent` must not become `x-bind:textcontent`). However,
#' pkgdown's underlying HTML parser (libxml2) lowercases all attributes.
#'
#' The `alpine_component` class enables automatic protection: when rendered
#' in vignettes, components are wrapped in a JavaScript shell that:
#' 1. Hides the HTML from pkgdown's parser
#' 2. Unwraps and renders interactively after the page loads
#' 3. Preserves all camelCase attributes exactly
#'
#' This protection is transparent to users—no code changes needed.
#'
#' @keywords internal
#' @examples
#' \donttest{
#' # Create an alpine component (class is automatic)
#' component <- alpine_tag("div", "Hello")
#' class(component)  # Returns: c("alpine_component", "shiny.tag", ...)
#'
#' # Print to console
#' print(component)
#'
#' # Open in RStudio Viewer for interactive preview
#' alpine_browser(component)
#' }
#'
NULL


#' Internal: Assign alpine_component class to tag objects
#'
#' @param tag An htmltools tag object to class
#' @return The tag with "alpine_component" prepended to class vector
#'
#' @keywords internal
#' @noRd
assign_alpine_class <- function(tag) {
  if (is.null(tag)) {
    return(tag)
  }
  
  # Add class only if not already present
  if (!inherits(tag, "alpine_component")) {
    class(tag) <- c("alpine_component", class(tag))
  }
  
  return(tag)
}
