#' Preview Alpine Component in RStudio Viewer
#'
#' Explicitly open an `alpine_component` in the RStudio Viewer pane for
#' interactive preview and testing. Useful for development and debugging.
#'
#' @param x An `alpine_component` object (from `alpine_tag()`, `alpine_data()`, etc.)
#' @param title Optional title to display in the Viewer tab. If `NULL`, uses
#'   "Alpine Component Preview".
#'
#' @return Invisibly returns `x` (for pipe chaining)
#'
#' @details
#'
#' ## When to Use
#'
#' - **Development:** Quickly preview a component while writing code
#' - **Testing:** Check interactive behavior with Alpine.js state changes
#' - **Debugging:** Inspect component rendering without full document context
#'
#' ## Usage in Console
#'
#' ```r
#' component <- alpine_tag("div",
#'   alpine_tag("button", "Click me"),
#'   alpine_data(count = 0)
#' )
#'
#' alpine_browser(component)  # Opens in Viewer pane
#' ```
#'
#' ## Usage in Vignettes
#'
#' Not typically needed in vignettes (components display directly), but can be
#' useful for explicit Viewer embedding:
#'
#' ```
#' alpine_browser(my_component)
#' ```
#'
#' @examples
#' \donttest{
#' # Simple button component
#' button <- alpine_tag("button", "Click",
#'   alpine_attr(`@click` = "count++")
#' ) |>
#'   alpine_data(count = 0)
#'
#' # Preview in RStudio Viewer
#' alpine_browser(button)
#'
#' # With custom title
#' alpine_browser(button, title = "Button Demo")
#' }
#'
#' @family Display Functions
#' @export
#' @importFrom htmltools renderTags
#' @importFrom utils browseURL
alpine_browser <- function(x, title = NULL) {
  # Validate input
  if (!inherits(x, "alpine_component")) {
    warning("x should be an alpine_component object. Attempting to render anyway.")
  }
  
  # Set default title
  if (is.null(title)) {
    title <- "Alpine Component Preview"
  }
  
  # Render the component to HTML
  rendered <- htmltools::renderTags(x)$html
  
  # Create a minimal HTML document with Alpine.js loaded
  html_doc <- sprintf(
    '<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s</title>
  <script defer src="https://cdn.jsdelivr.net/npm/alpinejs@3.15.8/dist/cdn.min.js"></script>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
      line-height: 1.6;
      color: #333;
      background-color: #f5f5f5;
      margin: 0;
      padding: 20px;
    }
    .alpine-component-container {
      background: white;
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 20px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      max-width: 800px;
      margin: 0 auto;
    }
    .alpine-component-title {
      margin-top: 0;
      color: #666;
      border-bottom: 2px solid #007bff;
      padding-bottom: 10px;
    }
  </style>
</head>
<body>
  <div class="alpine-component-container">
    <h2 class="alpine-component-title">%s</h2>
    %s
  </div>
</body>
</html>',
    title,
    title,
    rendered
  )
  
  # Write to temporary file
  temp_file <- tempfile(fileext = ".html")
  writeLines(html_doc, temp_file)
  
  # Open in RStudio Viewer if available (RStudio sets the "viewer" option),
  # otherwise fall back to default browser. Works in RStudio, VS Code,
  # and other IDEs that set this option.
  viewer_func <- getOption("viewer")
  if (!is.null(viewer_func)) {
    tryCatch(
      viewer_func(temp_file),
      error = function(e) {
        # Fall back to browser if viewer call fails
        utils::browseURL(temp_file)
      }
    )
  } else {
    # Fall back to browser if no viewer option set
    utils::browseURL(temp_file)
  }
  
  # Return invisibly for piping
  invisible(x)
}
