# Site configuration and navigation structure

# Guide pages: display_name -> source_filename
site_guides <- list(
  "Getting Started" = "getting-started",
  "Input Modifiers" = "input-modifiers",
  "Components & Patterns" = "components-and-patterns",
  "Advanced JSON" = "advanced-json",
  "Plugin Ecosystem" = "plugin-ecosystem",
  "Crosstalk Integration" = "crosstalk-integration",
  "Performance Guide" = "performance-guide",
  "Reactivity & Globals" = "reactivity-and-globals"
)

# Example pages: display_name -> source_filename
site_examples <- list(
  "TODO List" = "01-todo-list",
  "Expense Tracker" = "02-expense-tracker",
  "Settings Panel" = "03-settings-panel",
  "Inventory Dashboard" = "04-inventory-dashboard",
  "Form Wizard" = "05-form-wizard",
  "Live Search" = "06-live-search",
  "Input Modifiers" = "07-input-modifiers",
  "Plugins" = "08-plugins",
  "Crosstalk Dashboard" = "09-crosstalk-dashboard"
)

# Navigation menu (appears on all pages)
site_nav <- list(
  home = list(label = "alpinejs", url = "/"),
  guides = list(label = "Guides", url = "/guides/", items = site_guides),
  examples = list(label = "Examples", url = "/examples/", items = site_examples),
  reference = list(label = "Reference", url = "/reference/"),
  github = list(label = "GitHub", url = "https://github.com/brownag/alpinejs")
)
