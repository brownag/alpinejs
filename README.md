---
title: alpinejs
output: markdown
---

# alpinejs: Alpine.js Integration for R

Compose complete JavaScript applications using R. Build interactive web apps with [Alpine.js](https://alpinejs.dev/) without writing JavaScript. Use R functions to generate the HTML, state, and logic.

All interactivity runs client-side in the browser. No server, no R runtime after generation, no round-trips. Write the entire app in R, deploy it as static HTML.

Use it for static sites, dashboards, and interactive documents built with R Markdown, Quarto, or standalone HTML.

## Features

- No build step (direct R → HTML via htmltools)
- Client-side reactivity with Alpine.js
- R-to-JSON serialization with type preservation
- Pre-built components (accordions, modals, tabs, tables)
- Plugin system for extensions
- Crosstalk support for linked widgets

## Installation

```r
# From GitHub
remotes::install_github("brownag/alpinejs")
```

## Quick Start: Reactive Counter

```r
library(alpinejs)

alpine_tag("div",
  style = "padding: 20px; max-width: 300px;",
  alpine_tag("h2", "Counter"),
  alpine_tag("div",
    style = "display: flex; gap: 10px; margin: 20px 0;",
    alpine_tag("button", "−", alpine_attr(`@click` = "count--"), style = "padding: 10px 20px;"),
    alpine_tag("div",
      alpine_attr(`x-text` = "count"),
      style = "padding: 10px 20px; font-weight: bold; min-width: 50px; text-align: center;"
    ),
    alpine_tag("button", "+", alpine_attr(`@click` = "count++"), style = "padding: 10px 20px;")
  ),
  alpine_tag("p",
    "Total clicks: ",
    alpine_tag("span", alpine_attr(`x-text` = "count"))
  )
) |>
  alpine_data(count = 0)
```

## Example: Two-Way Data Binding

```r
library(alpinejs)

alpine_tag("div",
  style = "max-width: 300px; padding: 20px;",
  alpine_tag("label",
    "Your name: ",
    style = "display: block; margin-bottom: 10px; font-weight: bold;"
  ),
  alpine_tag("input",
    type = "text",
    placeholder = "Enter your name",
    style = "display: block; width: 100%; padding: 8px; margin-bottom: 15px; border: 1px solid #ddd; border-radius: 4px;"
  ) |>
    alpine_model("name"),
  alpine_tag("p",
    "Hello, ",
    alpine_tag("strong", alpine_attr(`x-text` = "name")),
    "!"
  )
) |>
  alpine_data(name = "")
```

## Example: Conditional Display

```r
library(alpinejs)

alpine_tag("div",
  style = "padding: 20px;",
  alpine_tag("button",
    "Show Details",
    style = "padding: 10px 20px; cursor: pointer;"
  ) |>
    alpine_on("click", "show = !show"),
  alpine_tag("div",
    style = "margin-top: 15px; padding: 15px; background: #f0f0f0; border-radius: 4px; border-left: 4px solid #007bff;",
    alpine_tag("p", "These are hidden details that appear when you click the button!")
  ) |>
    alpine_show("show")
) |>
  alpine_data(show = FALSE)
```

## Guides and Documentation

Learn alpinejs with our comprehensive guide suite:

- [Getting Started](https://brownag.github.io/alpinejs/guides/getting-started.html) - Start here for concepts and first examples
- [Input Modifiers](https://brownag.github.io/alpinejs/guides/input-modifiers.html) - Form control enhancements (debounce, lazy, etc.)
- [Components & Patterns](https://brownag.github.io/alpinejs/guides/components-and-patterns.html) - Pre-built UI components
- [Advanced JSON](https://brownag.github.io/alpinejs/guides/advanced-json.html) - Data serialization and type conversion
- [Plugin Ecosystem](https://brownag.github.io/alpinejs/guides/plugin-ecosystem.html) - Extending Alpine with plugins
- [Crosstalk Integration](https://brownag.github.io/alpinejs/guides/crosstalk-integration.html) - Cross-widget reactivity
- [Performance Guide](https://brownag.github.io/alpinejs/guides/performance-guide.html) - Optimization and best practices

## Examples

See runnable example applications:

- [Todo List](https://brownag.github.io/alpinejs/examples/01-todo-list.html)
- [Expense Tracker](https://brownag.github.io/alpinejs/examples/02-expense-tracker.html)
- [Settings Panel](https://brownag.github.io/alpinejs/examples/03-settings-panel.html)
- [Inventory Dashboard](https://brownag.github.io/alpinejs/examples/04-inventory-dashboard.html)
- [Form Wizard](https://brownag.github.io/alpinejs/examples/05-form-wizard.html)
- [Live Search](https://brownag.github.io/alpinejs/examples/06-live-search.html)
- [Input Modifiers](https://brownag.github.io/alpinejs/examples/07-input-modifiers.html)
- [Plugins](https://brownag.github.io/alpinejs/examples/08-plugins.html)
- [Crosstalk Dashboard](https://brownag.github.io/alpinejs/examples/09-crosstalk-dashboard.html)

## Function Reference

Browse the [complete function reference](https://brownag.github.io/alpinejs/reference/) with documentation for all exported functions.

## Core Functions

### Modifiers

Functions that enhance htmltools tags with Alpine.js directives:

- **`alpine_data(...)`** -- Initialize reactive state
- **`alpine_on(event, action)`** -- Attach event listeners
- **`alpine_show(condition)`** -- Conditionally show/hide elements
- **`alpine_bind(attribute, value)`** -- Dynamically bind HTML attributes
- **`alpine_model(variable)`** -- Two-way input binding

### Components

Pre-built interactive components:

- **`alpine_accordion(items)`** -- Collapsible panels
- **`alpine_modal(trigger_label, title, content)`** -- Modal dialog
- **`alpine_tabs(tabs)`** -- Tabbed interface
- **`alpine_field(...)`** -- Form field with label and error handling
- **`alpine_button(...)`** -- Button with state binding

### Helpers

Functions that generate Alpine.js expressions:

- **`alpine_array()`** -- Create empty JavaScript array
- **`alpine_object(...)`** -- Create JavaScript object
- **`alpine_validate_email(field)`** -- Email validation function
- **`alpine_storage_get(key, default)`** -- Get from localStorage
- **`alpine_storage_set(key, value_expr)`** -- Store to localStorage

## Input Modifiers

The `alpine_model()` function supports directives for common input patterns:

```r
# Lazy: Update on blur instead of every keystroke
alpine_tag("input") |> alpine_model("email", .lazy = TRUE)

# Debounce: Wait 300ms after typing stops
alpine_tag("input") |> alpine_model("search", .debounce = 300)

# Number: Auto-convert string to numeric
alpine_tag("input") |> alpine_model("age", .number = TRUE)

# Trim: Strip whitespace
alpine_tag("input") |> alpine_model("username", .trim = TRUE)

# Combine: Multiple modifiers together
alpine_tag("textarea") |> alpine_model("text", .lazy = TRUE, .trim = TRUE)
```

## Plugin System

Extend Alpine.js with additional functionality:

```r
library(alpinejs)

# Register built-in plugins
setup_persist()   # localStorage support
setup_mask()      # Input formatting
setup_sort()      # Array sorting

# Check registered plugins
get_plugin_registry()
```

## Crosstalk Integration

Build synchronized dashboards combining Alpine with other R widgets:

```r
library(alpinejs)
library(crosstalk)

# Create shared data
shared_data <- SharedData$new(iris, group = "demo")

# Create reactive table
alpine_crosstalk_table(shared_data, height = "400px")

# Selection syncs with other Crosstalk widgets (Plotly, Leaflet, etc.)
```

## Type Conversion

alpinejs automatically converts R types to JSON:

| R Type | JSON |
|--------|------|
| `TRUE` / `FALSE` | `true` / `false` |
| Factor | Character |
| Date | ISO 8601 string |
| POSIXct | ISO 8601 datetime |
| `NA` | `null` |
| `list(a=1)` | Object `{a:1}` |

## JavaScript Expressions

Use `htmlwidgets::JS()` to inject raw JavaScript:

```r
alpine_tag("div") |>
  alpine_data(
    message = "Hello",
    greet = htmlwidgets::JS("function() { alert(this.message); }")
  )
```

## CSP Compliance

By default, alpinejs uses Alpine.js's CSP-compliant build. To use the standard build:

```r
options(alpinejs.use_csp = FALSE)
```

## License

MIT

## Contributing

Contributions welcome! Please open an issue or PR on GitHub.

## See Also

### Alpine.js Resources
- [Alpine.js Official Site](https://alpinejs.dev/) - Learn Alpine.js directives and patterns
- [Alpine.js on GitHub](https://github.com/alpinejs/alpine) - Source code and community
- [Alpine.js Documentation](https://alpinejs.dev/start-here) - Complete API reference

### Related R Packages
- [htmltools](https://rstudio.github.io/htmltools/) - Building HTML from R
- [htmlwidgets](https://www.htmlwidgets.org/) - Creating R bindings to JavaScript libraries
- [crosstalk](https://rstudio.github.io/crosstalk/) - Cross-widget communication
- [Shiny](https://shiny.rstudio.com/) - For server-side interactivity
