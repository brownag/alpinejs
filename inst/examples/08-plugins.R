#' Alpine Plugin System Example: Persist, Mask, and Sort
#'
#' This example demonstrates how to use and configure Alpine.js plugins
#' with alpinejs. Plugins extend Alpine.js functionality including:
#' - localStorage persist: Auto-save component state
#' - Input masking: Format phone numbers, dates, etc.
#' - Array sorting: Dynamic sorting of lists/tables
#'
#' @keywords examples
#' @export

library(htmltools)
library(htmlwidgets)
library(alpinejs)

save_output <- interactive()

# ============================================================================
# Example 1: Persist Plugin - Save User Preferences to localStorage
# ============================================================================

# Register the persist plugin
alpine_setup_persist()

example_persist <- alpine_tag("div",
  alpine_tag("h2","Persist Plugin Example"),
  alpine_tag("p","Your theme preference is saved to localStorage automatically."),

  alpine_tag("div",
    class = "controls",
    alpine_tag("label",
      alpine_tag("input",type = "radio", name = "theme", value = "light") |>
        alpine_on("change", "saveTheme('light')"),
      "Light Theme"
    ),
    alpine_tag("label",
      alpine_tag("input",type = "radio", name = "theme", value = "dark") |>
        alpine_on("change", "saveTheme('dark')"),
      "Dark Theme"
    ),
    alpine_tag("label",
      alpine_tag("input",type = "radio", name = "theme", value = "auto") |>
        alpine_on("change", "saveTheme('auto')"),
      "Auto"
    )
  ),

  alpine_tag("div",
    "Current theme: ", 
    alpine_tag("strong",
      "x-text" = "theme"
    )
  )
) |>
  alpine_data(
    theme = htmlwidgets::JS("localStorage.getItem('theme') || 'light'"),
    saveTheme = htmlwidgets::JS("function(value) {
      this.theme = value;
      localStorage.setItem('theme', value);
    }")
  ) |>
  alpine_bind("data-theme", "theme")

# ============================================================================
# Example 2: Mask Plugin - Format Input Fields
# ============================================================================

# Register the mask plugin
alpine_setup_mask()

example_mask <- alpine_tag("div",
  alpine_tag("h2","Mask Plugin Example"),
  alpine_tag("p","Input fields are automatically formatted as you type."),

  alpine_tag("div",
    class = "form-group",
    alpine_tag("label","Phone Number (US Format)"),
    alpine_tag("input",
      type = "text",
      placeholder = "(999) 999-9999",
      "@input" = "phone = formatPhone($event.target.value)"
    ) |>
      alpine_model("phone")
  ),

  alpine_tag("div",
    class = "form-group",
    alpine_tag("label","Date (MM/DD/YYYY)"),
    alpine_tag("input",
      type = "text",
      placeholder = "MM/DD/YYYY",
      "@input" = "birthDate = formatDate($event.target.value)"
    ) |>
      alpine_model("birthDate")
  ),

  alpine_tag("div",
    class = "form-group",
    alpine_tag("label","Credit Card"),
    alpine_tag("input",
      type = "text",
      placeholder = "9999 9999 9999 9999",
      "@input" = "cardNumber = formatCard($event.target.value)"
    ) |>
      alpine_model("cardNumber")
  )
) |>
  alpine_data(
    phone = "",
    birthDate = "",
    cardNumber = "",
    formatPhone = htmlwidgets::JS("function(val) {
      const digits = val.replace(/\\D/g, '').slice(0, 10);
      if (digits.length === 0) return '';
      if (digits.length <= 3) return '(' + digits;
      if (digits.length <= 6) return '(' + digits.slice(0,3) + ') ' + digits.slice(3);
      return '(' + digits.slice(0,3) + ') ' + digits.slice(3,6) + '-' + digits.slice(6,10);
    }"),
    formatDate = htmlwidgets::JS("function(val) {
      const digits = val.replace(/\\D/g, '').slice(0, 8);
      if (digits.length === 0) return '';
      if (digits.length <= 2) return digits;
      if (digits.length <= 4) return digits.slice(0,2) + '/' + digits.slice(2);
      return digits.slice(0,2) + '/' + digits.slice(2,4) + '/' + digits.slice(4,8);
    }"),
    formatCard = htmlwidgets::JS("function(val) {
      const digits = val.replace(/\\D/g, '').slice(0, 16);
      if (digits.length === 0) return '';
      return digits.match(/.{1,4}/g).join(' ');
    }")
  )

# ============================================================================
# Example 3: Sort Plugin - Dynamic Array Sorting
# ============================================================================

# Register the sort plugin
alpine_setup_sort()

example_sort <- alpine_tag("div",
  alpine_tag("h2","Sort Plugin Example"),
  alpine_tag("p","Click column headers to sort the table."),

  alpine_tag("style", htmltools::HTML("
    table { border-collapse: collapse; width: 100%; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #f5f5f5; cursor: pointer; user-select: none; }
    th:hover { background-color: #e0e0e0; }
    .sort-indicator { font-size: 12px; margin-left: 5px; }
  ")),

  alpine_tag("table",
    alpine_tag("thead",
      alpine_tag("tr",
        alpine_tag("th",
          "Name",
          alpine_tag("span",class = "sort-indicator") |>
            alpine_bind("style", "{opacity: sortBy === 'name' ? 1 : 0.3}")
        ) |> alpine_on("click", "sortBy = sortBy === 'name' ? null : 'name'"),
        alpine_tag("th",
          "Age",
          alpine_tag("span",class = "sort-indicator") |>
            alpine_bind("style", "{opacity: sortBy === 'age' ? 1 : 0.3}")
        ) |> alpine_on("click", "sortBy = sortBy === 'age' ? null : 'age'"),
        alpine_tag("th",
          "City",
          alpine_tag("span",class = "sort-indicator") |>
            alpine_bind("style", "{opacity: sortBy === 'city' ? 1 : 0.3}")
        ) |> alpine_on("click", "sortBy = sortBy === 'city' ? null : 'city'")
      )
    ),
    alpine_tag("tbody",
      lapply(seq_len(3), function(i) {
        names <- c("Alice", "Bob", "Charlie")
        ages <- c(28, 34, 25)
        cities <- c("New York", "Los Angeles", "Chicago")
        alpine_tag("tr",
          "x-show" = paste0("getSortedItems()[", i-1, "]"),
          alpine_tag("td",
            "x-text" = paste0("getSortedItems()[", i-1, "].name")
          ),
          alpine_tag("td",
            "x-text" = paste0("getSortedItems()[", i-1, "].age")
          ),
          alpine_tag("td",
            "x-text" = paste0("getSortedItems()[", i-1, "].city")
          )
        )
      })
    )
  )
) |>
  alpine_data(
    sortBy = htmlwidgets::JS("null"),
    items = list(
      list(name = "Alice", age = 28, city = "New York"),
      list(name = "Bob", age = 34, city = "Los Angeles"),
      list(name = "Charlie", age = 25, city = "Chicago")
    ),
    getSortedItems = htmlwidgets::JS("function() {
      const sorted = [...this.items];
      if (!this.sortBy) return sorted;
      return sorted.sort((a, b) => {
        const aVal = a[this.sortBy];
        const bVal = b[this.sortBy];
        if (typeof aVal === 'number') return aVal - bVal;
        return String(aVal).localeCompare(String(bVal));
      });
    }")
  )

# ============================================================================
# Example 4: Multiple Plugins Working Together
# ============================================================================

example_combined <- alpine_tag("div",
  alpine_tag("h2","Combined Plugin Example"),
  alpine_tag("p","Using persist, mask, and sort together in one app."),

  # Settings section with persist
  alpine_tag("div",
    alpine_tag("h3","Settings (Persisted)"),
    alpine_tag("label",
      alpine_tag("input",
        type = "checkbox",
        "@change" = "saveSettings()"
      ) |> alpine_model("settings.notifications"),
      "Enable Notifications"
    ),
    alpine_tag("label",
      alpine_tag("select",
        alpine_tag("option",value = "en", "English"),
        alpine_tag("option",value = "es", "Spanish"),
        alpine_tag("option",value = "fr", "French"),
        "@change" = "saveSettings()"
      ) |> alpine_model("settings.language"),
      "Language: ",
      alpine_tag("span","x-text" = "settings.language")
    )
  ),

  # Contact form with masking
  alpine_tag("div",
    alpine_tag("h3","Add Contact (Masked Input)"),
    alpine_tag("input",
      type = "text",
      placeholder = "(999) 999-9999",
      "@input" = "newContact.phone = formatPhone($event.target.value)"
    ) |>
      alpine_model("newContact.phone", .debounce = 300),
    alpine_tag("button",
      "Add Contact"
    ) |> alpine_on("click", "addContact()")
  ),

  # Contacts list with sorting
  alpine_tag("div",
    alpine_tag("h3","Contacts"),
    alpine_tag("table",
      style = "width: 100%; border-collapse: collapse; margin-top: 10px;",
      alpine_tag("thead",
        alpine_tag("tr",
          alpine_tag("th",
            "Phone",
            style = "border: 1px solid #ddd; padding: 8px; background: #f5f5f5; cursor: pointer;",
            "@click" = "sortBy = sortBy === 'phone' ? null : 'phone'"
          ),
          alpine_tag("th",
            "Added",
            style = "border: 1px solid #ddd; padding: 8px; background: #f5f5f5; cursor: pointer;",
            "@click" = "sortBy = sortBy === 'date' ? null : 'date'"
          )
        )
      ),
      alpine_tag("tbody",
        lapply(seq_len(5), function(i) {
          alpine_tag("tr",
            "x-show" = paste0("getSortedContacts()[", i-1, "]"),
            "x-key" = paste0("i"),
            alpine_tag("td",
              "x-text" = paste0("getSortedContacts()[", i-1, "].phone"),
              style = "border: 1px solid #ddd; padding: 8px;"
            ),
            alpine_tag("td",
              "x-text" = paste0("getSortedContacts()[", i-1, "].date"),
              style = "border: 1px solid #ddd; padding: 8px;"
            )
          )
        })
      )
    )
  )
) |>
  alpine_data(
    settings = htmlwidgets::JS("JSON.parse(localStorage.getItem('settings') || '{\"notifications\":false,\"language\":\"en\"}')"),
    newContact = list(phone = ""),
    contacts = htmlwidgets::JS("JSON.parse(localStorage.getItem('contacts') || '[]')"),
    sortBy = htmlwidgets::JS("null"),
    formatPhone = htmlwidgets::JS("function(val) {
      const digits = val.replace(/\\D/g, '').slice(0, 10);
      if (digits.length === 0) return '';
      if (digits.length <= 3) return '(' + digits;
      if (digits.length <= 6) return '(' + digits.slice(0,3) + ') ' + digits.slice(3);
      return '(' + digits.slice(0,3) + ') ' + digits.slice(3,6) + '-' + digits.slice(6,10);
    }"),
    addContact = htmlwidgets::JS("function() {
      if (!this.newContact.phone || this.newContact.phone.length < 14) {
        alert('Please enter a valid phone number');
        return;
      }
      this.contacts.push({
        phone: this.newContact.phone,
        date: new Date().toLocaleDateString()
      });
      this.newContact.phone = '';
      this.saveSettings();
    }"),
    saveSettings = htmlwidgets::JS("function() {
      localStorage.setItem('settings', JSON.stringify(this.settings));
      localStorage.setItem('contacts', JSON.stringify(this.contacts));
    }"),
    getSortedContacts = htmlwidgets::JS("function() {
      const sorted = [...this.contacts];
      if (!this.sortBy) return sorted;
      return sorted.sort((a, b) => {
        const aVal = a[this.sortBy];
        const bVal = b[this.sortBy];
        if (typeof aVal === 'number') return aVal - bVal;
        return String(aVal).localeCompare(String(bVal));
      });
    }")
  )

# ============================================================================
# Create a demo page
# ============================================================================

# Save all examples to HTML files if running interactively
if (save_output) {
  htmltools::save_html(example_persist, "08-plugins-persist.html")
  htmltools::save_html(example_mask, "08-plugins-mask.html")
  htmltools::save_html(example_sort, "08-plugins-sort.html")
  htmltools::save_html(example_combined, "08-plugins-combined.html")
  cat("Saved plugin examples to current directory\n")
}
