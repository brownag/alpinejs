#' S3 Print Methods for Alpine Components
#'
#' Handle rendering of `alpine_component` objects in different contexts.
#' These methods provide automatic protection against HTML attribute lowercasing
#' in pkgdown by wrapping components in interactive JavaScript.
#'
#' @param x An `alpine_component` object (htmltools tag)
#' @param ... Additional arguments (passed to default methods)
#' @param options List of knitr options (passed by knitr)
#'
#' @return For `print()`: invisibly returns the tag; for `knit_print()`: returns
#'   HTML that knitr will include in output.
#'
#' @keywords internal
#' @name print.alpine_component
NULL


#' @rdname print.alpine_component
#' @method print alpine_component
#' @export
#' @importFrom htmltools renderTags
print.alpine_component <- function(x, ...) {
  # Render the tag to HTML string
  rendered <- htmltools::renderTags(x)
  
  # Print the HTML to console
  cat(rendered$html)
  
  # Return invisibly
  invisible(x)
}

if (!exists("knit_print", inherits = FALSE)) {
  knit_print <- function(x, ...) UseMethod("knit_print")
}

#' @rdname print.alpine_component
#' @method knit_print alpine_component
#' @export
#' @importFrom htmltools renderTags
knit_print.alpine_component <- function(x, options, ...) {
  # Render the tag to HTML with dependencies
  rendered <- htmltools::renderTags(x)

  # Get component HTML
  # Note: UTF-8 characters may be HTML-escaped by litedown's markdown processor
  # These will be unescaped in build_docs.R post-processing
  html_content <- as.character(rendered$html)

  # Return as knit_asis to insert verbatim into document
  # This produces live interactive widgets instead of code blocks
  # knitr::asis_output(html_content)
  structure(x, class = "knit_asis", knit_meta = NULL, knit_cacheable = NA)
}
