#' Create HTML tags using Alpine-friendly shorthand
#'
#' Convenience wrapper around `htmltools::tags` that provides a simpler interface
#' for creating HTML elements in vignettes and examples.
#' 
#' @param tag Character string naming the HTML tag (e.g., "div", "button", "p"). Supports all standard HTML5 tags.
#'
#' @param ... Arguments passed to the tag function. Supports regular HTML attributes,
#'   child tags, and Alpine directives via `alpine_attr()`. See details.
#'
#' @return An HTML tag object (shiny.tag) with Alpine directives applied.
#'
#' @details
#' This function provides a convenient way to create HTML tags without needing
#' to use `htmltools::tags$button()` syntax.
#'
#' **Alpine directives:** Pass directives using `alpine_attr()` to keep them
#' organized and separate from HTML attributes:
#'
#' ```
#' alpine_tag("button", "Click",
#'   alpine_attr(
#'     `@click` = "count++",
#'     `x-show` = "isModal"
#'   )
#' )
#' ```
#'
#' Common tags used with Alpine.js examples:
#' - `alpine_tag("div", ...)`
#' - `alpine_tag("button", ...)`
#' - `alpine_tag("input", type = "text", ...)`
#' - `alpine_tag("p", ...)`
#' - `alpine_tag("span", ...)`
#' - `alpine_tag("h1", ...)`, `alpine_tag("h2", ...)`, etc.
#'
#' @examples
#' \donttest{
#' # Create a simple button
#' alpine_tag("button", "Click me")
#'
#' # Nest tags
#' alpine_tag("div",
#'   alpine_tag("h2", "Title"),
#'   alpine_tag("p", "Some content")
#' )
#'
#' # Use with Alpine directives
#' alpine_tag("button", "Toggle",
#'   alpine_attr(`@click` = "show = !show")
#' )
#' }
#'
#' @export
#' @importFrom htmltools tags
alpine_tag <- function(tag, ...) {
  if (is.null(tag)) {
    stop("'tag' must be one of the following: ",
         toString(names(htmltools::tags)), call. = FALSE)
  }
  
  # Collect all arguments with their names preserved
  args <- list(...)
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))
  
  # Extract alpine_attr objects and keep remaining args with proper names
  alpine_directives <- list()
  remaining_args <- list()
  remaining_names <- character()
  
  for (i in seq_along(args)) {
    arg <- args[[i]]
    if (inherits(arg, "alpine_attr")) {
      # Collect Alpine directives
      alpine_directives <- c(alpine_directives, arg)
    } else {
      # Keep regular args with their names
      remaining_args <- c(remaining_args, list(arg))
      remaining_names <- c(remaining_names, arg_names[i])
    }
  }
  
  # Assign names to remaining args
  names(remaining_args) <- remaining_names
  
  # Create the tag with remaining arguments
  tag_fn <- htmltools::tags[[tag]]
  result_tag <- do.call(tag_fn, remaining_args)
  
  # Apply Alpine directives using alpine_append_attributes
  if (length(alpine_directives) > 0) {
    result_tag <- do.call(
      alpine_append_attributes,
      c(list(result_tag), alpine_directives)
    )
  }
  
  # Assign alpine_component class for S3 method dispatch
  result_tag <- assign_alpine_class(result_tag)
  
  return(result_tag)
}

#' Append attributes to an HTML tag
#'
#' Add HTML attributes to a tag object, preserving attribute name case.
#' Useful for Alpine.js directives and custom JavaScript attributes that
#' require specific casing (e.g., `x-bind:textContent`, `@click`).
#'
#' **Note:** Most users should use `alpine_attr()` with `alpine_tag()` for
#' organized directive assignment. Use this function for low-level attribute
#' manipulation directly on tags.
#'
#' @param tag An htmltools tag object.
#' @param ... Attributes to append. Can be:
#'   - Named arguments: `tag, .attr1 = value1, .attr2 = value2`
#'   - A single named character vector: `c("attr1" = "value1", "attr2" = "value2")`
#'
#' @return The modified tag with attributes appended.
#'
#' @details
#' Unlike `htmltools::tagAppendAttributes()`, this function preserves
#' the case of attribute names. This is essential for Alpine.js directives
#' and custom data attributes where case matters.
#'
#' **Difference from `alpine_attr()`:**
#' - `alpine_append_attributes()` - Direct tag manipulation, low-level
#' - `alpine_attr()` - High-level, for use with `alpine_tag()`
#'
#' @examples
#' \donttest{
#' # Low-level attribute manipulation
#' tag <- htmltools::tags$div("Content")
#' tag |>
#'   alpine_append_attributes("x-bind:title" = "myVar", "@click" = "handleClick()")
#'
#' # With high-level alpine_tag() - preferred
#' alpine_tag("div", "Content",
#'   alpine_attr(
#'     `x-bind:title` = "myVar",
#'     `@click` = "handleClick()"
#'   )
#' )
#' }
#'
#' @family Tag Utilities
#' @export
alpine_append_attributes <- function(tag, ...) {
  attrs <- list(...)
  
  # If single unnamed argument that's a named vector/list, use it directly
  if (length(attrs) == 1 && is.null(names(attrs))) {
    single_arg <- attrs[[1]]
    if (is.list(single_arg) || is.atomic(single_arg)) {
      if (!is.null(names(single_arg))) {
        attrs <- as.list(single_arg)
      }
    }
  }
  
  # Add each attribute directly to the tag's attribs list, preserving case
  for (attr_name in names(attrs)) {
    tag$attribs[[attr_name]] <- attrs[[attr_name]]
  }
  
  # Assign alpine_component class for S3 method dispatch
  tag <- assign_alpine_class(tag)
  
  return(tag)
}

#' Alpine.js directives and attributes
#'
#' Create a set of Alpine.js directives (e.g., `@click`, `x-show`, `x-model`)
#' to be added to a tag. This provides an explicit, organized way to attach
#' multiple Alpine directives while keeping HTML attributes separate.
#'
#' @param ... Named arguments representing Alpine directives.
#'   Attribute names should use Alpine syntax (e.g., `@click`, `x-show`, `x-model`).
#'
#' @return An object of class `alpine_attr` containing the directives.
#'   This can be passed to `alpine_tag()` as an argument.
#'
#' @details
#' `alpine_attr()` is a convenience function for organizing Alpine directives
#' when building tags with `alpine_tag()`. It wraps directives so they can be
#' cleanly separated from HTML attributes in your code.
#'
#' For low-level attribute manipulation (where case sensitivity matters),
#' use `alpine_append_attributes()` directly.
#'
#' @examples
#' \donttest{
#' # Directives with alpine_attr()
#' alpine_tag("button", "Click me",
#'   alpine_attr(
#'     `@click` = "count++",
#'     `x-show` = "isVisible"
#'   )
#' ) |>
#'   alpine_data(count = 0, isVisible = TRUE)
#'
#' # Multiple directives on same element
#' alpine_tag("input",
#'   type = "text",
#'   placeholder = "Search",
#'   alpine_attr(
#'     `x-model` = "query",
#'     `@input.debounce` = "search()",
#'     `@keydown.enter` = "submit()"
#'   )
#' )
#' }
#'
#' @family Tag Utilities
#' @export
alpine_attr <- function(...) {
  attrs <- list(...)
  
  # Validate that all arguments are named
  if (is.null(names(attrs)) || any(names(attrs) == "")) {
    stop("alpine_attr() requires all arguments to be named (e.g., `@click` = '...')")
  }
  
  # Create object with class alpine_attr for recognition by alpine_tag()
  structure(
    attrs,
    class = "alpine_attr"
  )
}
