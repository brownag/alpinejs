#' .onLoad hook for alpinejs
#'
#' Sets default package options and initializes the plugin registry when alpinejs is loaded.
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Set default CSP option to FALSE (standard Alpine.js for full feature support)
  # Users can opt-in to CSP mode with: options(alpinejs.use_csp = TRUE)
  if (is.null(getOption("alpinejs.use_csp"))) {
    options(alpinejs.use_csp = FALSE)
  }

  # Initialize Alpine.js version (used by plugins)
  if (is.null(getOption("alpinejs.version"))) {
    options(alpinejs.version = "3.15.8")
  }

  # Initialize plugin registry (empty list)
  if (is.null(getOption("alpinejs.plugins"))) {
    options(alpinejs.plugins = list())
  }

  # Auto-register crosstalk support if crosstalk is available
  tryCatch(
    {
      if (requireNamespace("crosstalk", quietly = TRUE)) {
        alpine_register_plugin(
          name = "crosstalk",
          version = "1.1.0",
          npm_scope = "crosstalk",
          required_alpine_version = "3.0"
        )
      }
    },
    error = function(e) {
      # Silently ignore if crosstalk registration fails
      NULL
    }
  )
}
