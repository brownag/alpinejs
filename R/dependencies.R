#' Control Alpine.js library version and Content Security Policy mode
#'
#' Configures which Alpine.js build to use: the standard version (with full JavaScript
#' support) or the CSP-compliant version (compatible with strict Content Security Policies).
#' Alpine.js is automatically included in generated HTML, but this function lets you
#' customize the version and security mode.
#'
#' **Most users don't need to call this directly.** The alpinejs package automatically
#' injects Alpine.js with sensible defaults. Use this function only if you need to:
#' - Switch between standard and CSP-compliant builds
#' - Use a specific Alpine.js version
#' - Manually attach Alpine to HTML templates
#'
#' @importFrom htmltools htmlDependency attachDependencies findDependencies tagAppendAttributes
#'
#' @param csp Logical. If `TRUE` (default), uses the CSP-compliant build
#'   (`@alpinejs/csp`) which disallows unsafe-eval and is suitable for strict
#'   Content Security Policies. If `FALSE`, uses the standard Alpine.js build
#'   with full JavaScript support. Can be set globally via
#'   `options(alpinejs.use_csp = FALSE)`.
#' @param version Character string specifying the Alpine.js version.
#'   Defaults to "3.15.8". Should be a valid semantic version available on
#'   the CDN.
#'
#' @return An `htmltools::htmlDependency` object containing the Alpine.js script.
#'   Useful for manually including Alpine.js in custom HTML templates. For regular
#'   alpinejs components, Alpine.js is automatically injected.
#'
#' @details
#' **Automatic Injection:** Alpine.js is automatically injected by functions like
#' `alpine_data()`, `alpine_on()`, and other modifiers. You typically do not need
#' to call this function.
#'
#' **Manual Use Cases:**
#' - Changing the Alpine.js version globally
#' - Switching CSP mode for your application
#' - Attaching Alpine.js to custom HTML templates
#'
#' **CDN:** Alpine.js is fetched from jsDelivr (https://www.jsdelivr.com/):
#' - **CSP mode** (`csp = TRUE`):
#'   `https://cdn.jsdelivr.net/npm/@alpinejs/csp@VERSION/dist/cdn.min.js`
#' - **Standard mode** (`csp = FALSE`):
#'   `https://cdn.jsdelivr.net/npm/alpinejs@VERSION/dist/cdn.min.js`
#'
#' **CSP Limitations:** The CSP build disallows certain JavaScript patterns
#' (functions, closures) to comply with strict Content Security Policies.
#' See [Alpine.js CSP documentation](https://alpinejs.dev/advanced/csp).
#'
#' @examples
#' \donttest{
#' # Use CSP-compliant build (default)
#' dep <- alpine_config(csp = TRUE)
#'
#' # Use standard build with full JavaScript support
#' dep <- alpine_config(csp = FALSE)
#'
#' # Use a different Alpine.js version
#' dep <- alpine_config(version = "3.14.0")
#' }
#'
#' @family Advanced Configuration
#' @export
alpine_config <- function(csp = getOption("alpinejs.use_csp", FALSE),
                          version = "3.15.8") {
  # Validate version format
  if (!is.character(version) || length(version) != 1) {
    stop("version must be a single character string (e.g., '3.15.8')")
  }

  # Construct the CDN URL based on CSP preference
  if (csp) {
    full_url <- paste0("https://cdn.jsdelivr.net/npm/@alpinejs/csp@", version, "/dist/cdn.min.js")
  } else {
    full_url <- paste0("https://cdn.jsdelivr.net/npm/alpinejs@", version, "/dist/cdn.min.js")
  }

  # Create dependency using htmltools::htmlDependency with CDN URL
  # The src points to the package directory (which always exists)
  # The script is delivered via the head parameter as a complete script tag
  script_html <- sprintf(
    '<script type="text/javascript" src="%s" defer></script>',
    full_url
  )

  # Get the package root directory - use "." as fallback
  # to ensure this works both in regular use and during package building
  pkg_root <- tryCatch(
    system.file(package = "alpineR"),
    error = function(e) "."
  )
  if (!nzchar(pkg_root)) pkg_root <- "."

  dep <- htmltools::htmlDependency(
    name = "alpine",
    version = version,
    src = pkg_root,
    script = NULL,
    stylesheet = NULL,
    head = script_html,
    meta = NULL,
    attachment = NULL,
    package = "alpineR",
    all_files = FALSE
  )

  # Store URL in a custom attribute for testing purposes
  # This allows tests to verify the correct CDN URL was selected
  attr(dep, "cdn_url") <- full_url
  
  return(dep)
}

#' @keywords internal
#' Internal utility: Check if Alpine.js dependency is already attached
#'
#' Used internally to avoid duplicate dependency injection.
#'
#' @param tag An htmltools tag object.
#'
#' @return Logical. `TRUE` if the Alpine.js dependency is already attached
#'   to the tag tree; `FALSE` otherwise.
#'
#' @noRd
is_alpine_attached <- function(tag) {
  deps <- htmltools::findDependencies(tag)
  any(vapply(deps, function(d) d$name == "alpine", logical(1)))
}

#' Ensure Alpine.js Dependency
#'
#' Internal helper that ensures an Alpine.js dependency is attached to a tag.
#' If already attached, does nothing. If not attached, attaches the default
#' dependency.
#'
#' @param tag An htmltools tag object.
#'
#' @return The tag with an Alpine.js dependency attached.
#'
#' @noRd
ensure_alpine_dependency <- function(tag) {
  if (!is_alpine_attached(tag)) {
    htmltools::attachDependencies(
      tag,
      alpine_config(),
      append = TRUE
    )
  } else {
    tag
  }
}
