#' Input Modifiers Example: Form with Debounced Search and Lazy Validation
#'
#' This example demonstrates Alpine.js input modifiers for common form patterns:
#' - Debounced search (waits 300ms after typing stops)
#' - Lazy validation (updates only on blur)
#' - Number conversion (auto-convert string to number)
#' - Trimmed input (strips whitespace)
#'
#' Run: Rscript inst/examples/07-input-modifiers.R

library(alpinejs)

save_output <- interactive()
library(htmltools)
library(htmlwidgets)

# Create the form using alpinejs functions
app <- alpine_tag("div",
  style = "max-width: 600px; margin: 40px auto; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;",
  
  alpine_tag("h1","Input Modifiers Demo"),
  alpine_tag("p","Try the different input modifiers and watch the state update in real-time."),

  # =====================================================================
  # 1. Debounced Search Input
  # =====================================================================
  alpine_tag("div",
    style = "margin-bottom: 20px;",
    alpine_tag("label","1. Debounced Search (300ms delay)", style = "display: block; font-weight: bold; margin-bottom: 5px;"),
    alpine_tag("input",
      type = "text",
      placeholder = "Type to search (waits 300ms after you stop)...",
      style = "width: 100%; padding: 8px; box-sizing: border-box; border: 1px solid #ccc; border-radius: 4px;"
    ) |>
      alpine_model("searchQuery", .debounce = 300),
    alpine_tag("div",
      style = "background: #f5f5f5; padding: 10px; border-radius: 4px; font-size: 14px; color: #666; margin-top: 5px;",
      "Uses modifiers: x-model.debounce.300",
      alpine_tag("div",
        style = "margin-top: 10px; font-size: 12px; color: #999;",
        "Current search: ",
        alpine_tag("span") |>
          alpine_text("searchQuery")
      )
    )
  ),

  # =====================================================================
  # 2. Lazy Validation Input
  # =====================================================================
  alpine_tag("div",
    style = "margin-bottom: 20px;",
    alpine_tag("label","2. Email Input (Lazy Validation)", style = "display: block; font-weight: bold; margin-bottom: 5px;"),
    alpine_tag("input",
      type = "text",
      placeholder = "example@email.com",
      style = "width: 100%; padding: 8px; box-sizing: border-box; border: 1px solid #ccc; border-radius: 4px;"
    ) |>
      alpine_model("emailInput", .lazy = TRUE),
    alpine_tag("div",
      style = "background: #f5f5f5; padding: 10px; border-radius: 4px; font-size: 14px; color: #666; margin-top: 5px;",
      "Uses modifiers: x-model.lazy",
      alpine_tag("div",
        style = "margin-top: 10px; font-size: 12px; color: #999;",
        "Email (updates on blur): ",
        alpine_tag("span") |>
          alpine_text("emailInput")
      ),
      alpine_tag("div",
        style = "margin-top: 10px; padding: 8px; border-radius: 4px;"
      ) |>
        alpine_bind("class", "emailInput && /^[^@]+@[^@]+\\.\\w+$/.test(emailInput) ? 'valid' : 'invalid'"),
      alpine_tag("div",
        style = "margin-top: 10px; font-size: 12px;"
      ) |>
        alpine_text("emailInput && /^[^@]+@[^@]+\\.\\w+$/.test(emailInput) ? 'Valid' : 'Invalid'") |>
        alpine_bind("style", "emailInput && /^[^@]+@[^@]+\\.\\w+$/.test(emailInput) ? 'color:green' : 'color:red'")
    )
  ),

  # =====================================================================
  # 3. Number Conversion Input
  # =====================================================================
  alpine_tag("div",
    style = "margin-bottom: 20px;",
    alpine_tag("label","3. Age Input (Auto-number Conversion)", style = "display: block; font-weight: bold; margin-bottom: 5px;"),
    alpine_tag("input",
      type = "text",
      placeholder = "Enter your age",
      style = "width: 100%; padding: 8px; box-sizing: border-box; border: 1px solid #ccc; border-radius: 4px;"
    ) |>
      alpine_model("userAge", .number = TRUE),
    alpine_tag("div",
      style = "background: #f5f5f5; padding: 10px; border-radius: 4px; font-size: 14px; color: #666; margin-top: 5px;",
      "Uses modifiers: x-model.number",
      alpine_tag("div",
        style = "margin-top: 10px; font-size: 12px; color: #999;",
        "Current age: ",
        alpine_tag("span") |>
          alpine_text("userAge"),
        " (type: ",
        alpine_tag("span") |>
          alpine_text("typeof userAge"),
        ")"
      )
    )
  ),

  # =====================================================================
  # 4. Trimmed Input
  # =====================================================================
  alpine_tag("div",
    style = "margin-bottom: 20px;",
    alpine_tag("label","4. Trimmed Input (whitespace automatically removed)", style = "display: block; font-weight: bold; margin-bottom: 5px;"),
    alpine_tag("input",
      type = "text",
      placeholder = "Type with spaces (they will be trimmed)",
      style = "width: 100%; padding: 8px; box-sizing: border-box; border: 1px solid #ccc; border-radius: 4px;"
    ) |>
      alpine_model("username", .trim = TRUE),
    alpine_tag("div",
      style = "background: #f5f5f5; padding: 10px; border-radius: 4px; font-size: 14px; color: #666; margin-top: 5px;",
      "Uses modifiers: x-model.trim",
      alpine_tag("div",
        style = "margin-top: 10px; font-size: 12px; color: #999;",
        'Username: "',
        alpine_tag("span") |>
          alpine_text("username"),
        '"'
      )
    )
  ),

  # =====================================================================
  # 5. Combined Modifiers
  # =====================================================================
  alpine_tag("div",
    style = "margin-bottom: 20px;",
    alpine_tag("label","5. Combined Modifiers (lazy + debounce + trim)", style = "display: block; font-weight: bold; margin-bottom: 5px;"),
    alpine_tag("textarea",
      placeholder = "Description (lazy + debounce.250 + trim)",
      style = "width: 100%; padding: 8px; box-sizing: border-box; border: 1px solid #ccc; border-radius: 4px; min-height: 100px;"
    ) |>
      alpine_model("description", .lazy = TRUE, .debounce = 250, .trim = TRUE),
    alpine_tag("div",
      style = "background: #f5f5f5; padding: 10px; border-radius: 4px; font-size: 14px; color: #666; margin-top: 5px;",
      "Uses modifiers: x-model.lazy.debounce.250.trim",
      alpine_tag("div",
        style = "margin-top: 10px; font-size: 12px; color: #999;",
        "Description length: ",
        alpine_tag("span") |>
          alpine_text("description.length"),
        " characters"
      )
    )
  ),

  # =====================================================================
  # 6. Current State Display
  # =====================================================================
  alpine_tag("div",
    style = "background: #e8f4f8; padding: 15px; border-radius: 4px; margin-top: 30px; font-family: monospace; white-space: pre-wrap; word-break: break-word; font-size: 12px;"
  ) |>
    alpine_text("JSON.stringify({ searchQuery, emailInput, userAge, username, description }, null, 2)")
) |>
  alpine_data(
    searchQuery = "",
    emailInput = "",
    userAge = htmlwidgets::JS("null"),
    username = "",
    description = ""
  )

html_content <- alpine_tag("html",
  alpine_tag("head",
    alpine_tag("title", "Input Modifiers Example"),
    alpine_tag("meta", charset = "utf-8"),
    alpine_tag("meta", name = "viewport", content = "width=device-width, initial-scale=1.0")
  ),
  alpine_tag("body",
    app
  ) |>
    htmltools::attachDependencies(
      htmltools::htmlDependency(
        name = "alpine",
        version = "3.15.8",
        src = c(href = "https://cdn.jsdelivr.net/npm/alpinejs@3.15.8/dist"),
        script = "cdn.min.js"
      )
    )
) |>
  htmltools::browsable()

# Save to file
if (save_output) {
  htmltools::save_html(html_content, "07-input-modifiers.html")
  cat("Saved to: 07-input-modifiers.html\n")
}
