#' Register an Alpine.js plugin for use with alpinejs
#'
#' Adds a plugin to the alpinejs registry. Plugins are JavaScript extensions from the
#' Alpine.js ecosystem (e.g., persist, mask, sort, etc.). Once registered, setup functions
#' like `alpine_setup_mask()` can use the plugin and its dependencies will be automatically
#' injected into generated HTML.
#'
#' @param name Character string naming the plugin (e.g., "persist", "mask").
#' @param version Character string specifying the plugin version. Must match
#'   the Alpine.js version (e.g., "3.15.8"). Default: uses installed Alpine.js
#'   version from `getOption("alpinejs.version")`.
#' @param npm_scope Character string for the npm package scope (default: "alpinejs").
#'   Used to construct CDN URLs. For example, "persist" with "alpinejs" scope
#'   becomes `@alpinejs/persist`.
#' @param required_alpine_version Character string specifying the minimum required
#'   Alpine.js version. Default: "3.0" (supports Alpine 3.x).
#' @param init_code Optional character string containing JavaScript code to
#'   execute when the plugin initializes. Useful for configuration.
#'
#' @return Invisibly returns `TRUE` if registration succeeds. Used for its
#'   side effect of adding the plugin to the registry.
#'
#' @details
#' **Plugin Registry:** Plugins are stored in `options("alpinejs.plugins")` as a
#' named list. Each entry contains metadata used to generate CDN URLs and
#' manage dependencies.
#'
#' **CDN URLs:** Plugin dependencies are automatically constructed as:
#' ```
#' https://cdn.jsdelivr.net/npm/@{npm_scope}/{name}@{version}/dist/{name}.min.js
#' ```
#'
#' **Duplicate Detection:** If a plugin with the same name is already registered,
#' a warning is issued and the existing registration is kept (no re-registration).
#'
#' **Version Validation:** Plugin versions must be SemVer format (e.g., "3.15.8").
#' Invalid versions produce a warning but don't prevent registration.
#'
#' @examples
#' \donttest{
#' alpine_register_plugin("persist", version = "3.15.8")
#' alpine_is_plugin_registered("persist")
#' }
#'
#' @family Plugin System
#' @export
alpine_register_plugin <- function(
  name,
  version = NULL,
  npm_scope = "alpinejs",
  required_alpine_version = "3.0",
  init_code = NULL
) {
  # Validate inputs
  if (!is.character(name) || length(name) != 1 || name == "") {
    stop("name must be a non-empty character string")
  }

  if (!is.character(npm_scope) || length(npm_scope) != 1) {
    stop("npm_scope must be a character string")
  }

  if (!is.character(required_alpine_version) || length(required_alpine_version) != 1) {
    stop("required_alpine_version must be a character string")
  }

  if (!is.null(init_code)) {
    if (!is.character(init_code) || length(init_code) != 1) {
      stop("init_code must be NULL or a character string")
    }
  }

  # Default version to package's Alpine version
  if (is.null(version)) {
    version <- getOption("alpinejs.version", default = "3.15.8")
  }

  if (!is.character(version) || length(version) != 1) {
    stop("version must be a character string (e.g., '3.15.8')")
  }

  # Validate version format (loose: accepts X.Y.Z format)
  if (!grepl("^\\d+\\.\\d+(\\.\\d+)?(-[a-zA-Z0-9.]+)?$", version)) {
    warning(
      "Plugin '", name, "' version '", version,
      "' doesn't match expected SemVer format (X.Y.Z)"
    )
  }

  # Get current registry
  registry <- getOption("alpinejs.plugins", default = list())

  # Check for duplicates
  if (name %in% names(registry)) {
    # Silently skip re-registration
    return(invisible(TRUE))
  }

  # Create plugin entry
  plugin <- list(
    name = name,
    version = version,
    npm_scope = npm_scope,
    required_alpine_version = required_alpine_version,
    init_code = init_code,
    registered_at = Sys.time()
  )

  # Add to registry
  registry[[name]] <- plugin

  # Update global option
  options(alpinejs.plugins = registry)

  invisible(TRUE)
}

#' Get the Current Plugin Registry
#'
#' Returns a list of all registered Alpine.js plugins with their metadata.
#' Useful for debugging and understanding which plugins are available.
#'
#' @return A named list where each element is a plugin. Each plugin contains:
#'   - `name`: Plugin name
#'   - `version`: Plugin version (SemVer)
#'   - `npm_scope`: npm package scope
#'   - `required_alpine_version`: Minimum Alpine.js version
#'   - `init_code`: Optional initialization JavaScript
#'   - `registered_at`: Timestamp of registration
#'
#'   Returns an empty list if no plugins are registered.
#'
#' @examples
#' \donttest{
#' alpine_get_plugin_registry()
#' }
#'
#' @family Plugin System
#' @export
alpine_get_plugin_registry <- function() {
  getOption("alpinejs.plugins", default = list())
}

#' Get Plugin CDN URL
#'
#' Constructs the jsDelivr CDN URL for a registered plugin.
#'
#' @param plugin_name Character string naming the plugin.
#'
#' @return Character string with the CDN URL for the plugin's JavaScript file.
#'   Or `NA_character_` if the plugin is not registered.
#'
#' @details
#' CDN URL format: `https://cdn.jsdelivr.net/npm/@{npm_scope}/{name}@{version}/dist/{name}.min.js`
#'
#' Example: `@alpinejs/persist@3.15.8` produces:
#' `https://cdn.jsdelivr.net/npm/@alpinejs/persist@3.15.8/dist/persist.min.js`
#'
#' @keywords internal
get_plugin_cdn_url <- function(plugin_name) {
  registry <- alpine_get_plugin_registry()

  if (!(plugin_name %in% names(registry))) {
    return(NA_character_)
  }

  plugin <- registry[[plugin_name]]
  sprintf(
    "https://cdn.jsdelivr.net/npm/@%s/%s@%s/dist/%s.min.js",
    plugin$npm_scope,
    plugin$name,
    plugin$version,
    plugin$name
  )
}

#' Create HTML Dependency for an Alpine Plugin
#'
#' Generates an htmltools dependency for a registered Alpine.js plugin.
#' This is used internally to inject plugin scripts into HTML documents.
#'
#' @param plugin_name Character string naming the plugin.
#'
#' @return An htmltools::htmlDependency object, or `NULL` if the plugin is
#'   not registered.
#'
#' @details
#' The dependency includes:
#' - Plugin script from jsDelivr CDN
#' - Dependency on Alpine.js core (ensures Alpine loads before plugin)
#' - Plugin-specific metadata
#'
#' @keywords internal
html_dependency_plugin <- function(plugin_name) {
  registry <- alpine_get_plugin_registry()

  if (!(plugin_name %in% names(registry))) {
    return(NULL)
  }

  plugin <- registry[[plugin_name]]
  cdn_url <- get_plugin_cdn_url(plugin_name)

  if (is.na(cdn_url)) {
    return(NULL)
  }

  htmltools::htmlDependency(
    name = paste0("alpine-plugin-", plugin$name),
    version = plugin$version,
    src = c(href = cdn_url),
    script = plugin$name,
    all_files = FALSE,
    meta = list(
      scope = plugin$npm_scope,
      plugin_name = plugin$name,
      alpine_version = getOption("alpinejs.version", "3.15.8")
    )
  )
}

#' Get Alpine Plugin Dependency
#'
#' Retrieves the htmltools dependency for a plugin, with automatic deduplication.
#' Returns the dependency if the plugin is registered, otherwise issues a warning
#' and returns NULL.
#'
#' @param plugin_name Character string naming the plugin.
#'
#' @return An htmltools::htmlDependency, or invisibly NULL if not registered.
#'
#' @keywords internal
ensure_plugin_dependency <- function(plugin_name) {
  registry <- alpine_get_plugin_registry()

  if (!(plugin_name %in% names(registry))) {
    warning(
      "Plugin '", plugin_name, "' is not registered. ",
      "Use register_alpine_plugin('", plugin_name, "') first."
    )
    return(invisible(NULL))
  }

  html_dependency_plugin(plugin_name)
}

#' Check if a Plugin is Registered
#'
#' Quickly check whether a named plugin has been registered.
#'
#' @param plugin_name Character string naming the plugin.
#'
#' @return Logical; `TRUE` if registered, `FALSE` otherwise.
#'
#' @examples
#' \donttest{
#' alpine_register_plugin("persist")
#' alpine_is_plugin_registered("persist")
#' }
#'
#' @family Plugin System
#' @export
alpine_is_plugin_registered <- function(plugin_name) {
  plugin_name %in% names(alpine_get_plugin_registry())
}

#' Pre-built Alpine.js Plugin Templates
#'
#' Convenience functions to set up common Alpine.js plugins with sensible defaults.
#' These are wrappers around `alpine_register_plugin()` that handle configuration.
#'
#' @details
#' **Persist Plugin** (`alpine_setup_persist`)
#' Enables browser localStorage integration for state persistence.
#' Components can use `@persist='varname'` to auto-save to localStorage.
#'
#' **Mask Plugin** (`alpine_setup_mask`)
#' Provides input masking (e.g., phone numbers: (999) 999-9999).
#' Components can use `x-mask='(999) 999-9999'` on inputs.
#'
#' **Sort Plugin** (`alpine_setup_sort`)
#' Adds array sorting to Alpine.js components.
#' Useful for dynamic table column sorting and list reordering.
#'
#' @name plugin_templates
#' @keywords internal
NULL

#' @rdname plugin_templates
#' @family Plugin System
#' @export
alpine_setup_persist <- function() {
  alpine_register_plugin(
    name = "persist",
    version = getOption("alpinejs.version", "3.15.8"),
    npm_scope = "alpinejs",
    init_code = NULL
  )
  invisible(TRUE)
}

#' @rdname plugin_templates
#' @family Plugin System
#' @export
alpine_setup_mask <- function() {
  alpine_register_plugin(
    name = "mask",
    version = getOption("alpinejs.version", "3.15.8"),
    npm_scope = "alpinejs",
    init_code = NULL
  )
  invisible(TRUE)
}

#' @rdname plugin_templates
#' @family Plugin System
#' @export
alpine_setup_sort <- function() {
  alpine_register_plugin(
    name = "sort",
    version = getOption("alpinejs.version", "3.15.8"),
    npm_scope = "alpinejs",
    init_code = NULL
  )
  invisible(TRUE)
}
