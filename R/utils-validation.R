#' @keywords internal
#' Validate CSS selector format
#'
#' @param selector A CSS selector string
#'
#' @return The selector, unmodified, or error if invalid
#'
#' @noRd
validate_css_selector <- function(selector) {
  # Basic validation - not exhaustive
  if (!is.character(selector) || length(selector) != 1) {
    stop("CSS selector must be a single character string")
  }
  selector
}

#' @keywords internal
#' Validate JavaScript event name
#'
#' @param event An event name string
#'
#' @return The event, unmodified, or error if invalid
#'
#' @noRd
validate_event_name <- function(event) {
  if (!is.character(event) || length(event) != 1) {
    stop("Event name must be a single character string")
  }
  event
}
