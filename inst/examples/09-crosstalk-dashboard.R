#' Crosstalk Integration Examples: Multi-Widget Reactivity
#'
#' This example demonstrates how to use Crosstalk with alpinejs to create
#' synchronized, reactive multi-widget dashboards. Widgets automatically filter
#' and select based on user interactions in other components.
#'
#' @keywords examples
#' @export

library(htmltools)
library(htmlwidgets)
library(alpinejs)

save_output <- interactive()

# Check if crosstalk is available
if (requireNamespace("crosstalk", quietly = TRUE)) {

# ============================================================================
# Example 1: Alpine Table + Plotly Selection Sync
# ============================================================================

example_alpine_plotly <- function() {
  library(crosstalk)

  # Create shared data
  shared_iris <- SharedData$new(iris, group = "iris_demo")

  # Create layout container
  alpine_tag("div",
    style = "display: flex; gap: 20px; padding: 20px;",

    # Left: Plotly scatter (if plotly available)
    alpine_tag("div",
      style = "flex: 1;",
      alpine_tag("h3","Plotly Scatter Plot (Click to select)"),
      if (requireNamespace("plotly", quietly = TRUE)) {
        # Would insert plotly widget here
        alpine_tag("div","Plotly chart would render here")
      } else {
        alpine_tag("div","plotly not installed")
      }
    ),

    # Right: Alpine Table
    alpine_tag("div",
      style = "flex: 1;",
      alpine_tag("h3","Alpine Crosstalk Table"),
      alpine_crosstalk_table(
        shared_iris,
        columns = c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length"),
        selectable = TRUE,
        height = "400px"
      )
    )
  )
}

# ============================================================================
# Example 2: Filter Widget + Reactive Table
# ============================================================================

example_filter_table <- function() {
  library(crosstalk)

  # Simple cars dataset for filtering demo
  cars_data <- data.frame(
    id = rownames(mtcars),
    mpg = mtcars$mpg,
    cyl = mtcars$cyl,
    hp = mtcars$hp,
    wt = mtcars$wt,
    row.names = NULL
  )

  alpine_tag("div",
    style = "padding: 20px;",

    alpine_tag("h2","Filter Cars by Specifications"),

    # Filter section
    alpine_tag("div",
      style = "background: #f5f5f5; padding: 15px; margin-bottom: 20px; border-radius: 4px;",

      alpine_tag("h3","Filters"),

      alpine_tag("div",
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",

        # MPG filter
        alpine_tag("div",
          alpine_tag("label","Fuel Efficiency (MPG):"),
          alpine_tag("input",
            type = "range",
            min = 10,
            max = 35,
            value = 20
          ) |>
            alpine_model("minMpg", .debounce = 300),
          alpine_tag("span",
            "Min: ",
            alpine_tag("span","x-text" = "minMpg"),
            " MPG"
          )
        ),

        # Cylinders filter
        alpine_tag("div",
          alpine_tag("label","Number of Cylinders:"),
          alpine_tag("select",
            alpine_tag("option",value = "", "All"),
            alpine_tag("option",value = "4", "4 cylinders"),
            alpine_tag("option",value = "6", "6 cylinders"),
            alpine_tag("option",value = "8", "8 cylinders")
          ) |>
            alpine_model("selectedCyl")
        )
      )
    ),

    # Results section
    alpine_tag("div",
      alpine_tag("h3",
        "Results: ",
        alpine_tag("span",
          "x-text" = "getFilteredCars().length"
        ),
        " cars"
      ),

      # Simple table without using alpine_crosstalk_table
      alpine_tag("table",
        style = "width: 100%; border-collapse: collapse;",
        alpine_tag("thead",
          alpine_tag("tr",
            alpine_tag("th",
              "Car",
              style = "padding: 8px; background: #f5f5f5; border: 1px solid #ddd; text-align: left;font-weight: bold;"
            ),
            alpine_tag("th",
              "MPG",
              style = "padding: 8px; background: #f5f5f5; border: 1px solid #ddd; text-align: left;font-weight: bold;"
            ),
            alpine_tag("th",
              "Cyl",
              style = "padding: 8px; background: #f5f5f5; border: 1px solid #ddd; text-align: left;font-weight: bold;"
            ),
            alpine_tag("th",
              "HP",
              style = "padding: 8px; background: #f5f5f5; border: 1px solid #ddd; text-align: left;font-weight: bold;"
            ),
            alpine_tag("th",
              "Weight",
              style = "padding: 8px; background: #f5f5f5; border: 1px solid #ddd; text-align: left;font-weight: bold;"
            )
          )
        ),
        alpine_tag("tbody",
          lapply(seq_len(nrow(cars_data)), function(i) {
            car_id <- as.character(cars_data$id[i])
            car_mpg <- cars_data$mpg[i]
            car_cyl <- cars_data$cyl[i]
            
            alpine_tag("tr",
              "x-show" = paste0("getFilteredCars().some(c => c.id === '", car_id, "')"),
              style = "border-bottom: 1px solid #ddd;",
              alpine_tag("td",car_id, style = "padding: 8px; border: 1px solid #ddd;"),
              alpine_tag("td",car_mpg, style = "padding: 8px; border: 1px solid #ddd;"),
              alpine_tag("td",car_cyl, style = "padding: 8px; border: 1px solid #ddd;"),
              alpine_tag("td",cars_data$hp[i], style = "padding: 8px; border: 1px solid #ddd;"),
              alpine_tag("td",round(cars_data$wt[i], 2), style = "padding: 8px; border: 1px solid #ddd;")
            )
          })
        )
      )
    )
  ) |>
    alpine_data(
      minMpg = 20,
      selectedCyl = "",
      carsData = jsonlite::toJSON(cars_data, dataframe = "rows"),
      getFilteredCars = htmlwidgets::JS("function() {
        const cars = ", jsonlite::toJSON(cars_data, dataframe = "rows"), ";
        return cars.filter(car => {
          const mpgMatch = car.mpg >= this.minMpg;
          const cylMatch = !this.selectedCyl || String(car.cyl) === this.selectedCyl;
          return mpgMatch && cylMatch;
        });
      }")
    )
}

# ============================================================================
# Example 3: Multi-Filter Dashboard with Summary
# ============================================================================

example_dashboard <- function() {
  library(crosstalk)

  # Create shared data with some preprocessing
  data <- data.frame(
    id = seq_len(100),
    category = sample(c("A", "B", "C"), 100, replace = TRUE),
    region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
    value = rnorm(100, mean = 50, sd = 15),
    date = Sys.Date() - sample(0:30, 100, replace = TRUE)
  )

  shared_data <- crosstalk::SharedData$new(data, group = "dashboard_demo")

  alpine_tag("div",
    style = "padding: 20px; background: white;",

    alpine_tag("h1","Crosstalk Dashboard"),
    alpine_tag("p","Select items in the table to update charts (when integrated with Plotly/Leaflet)."),

    # Control panel
    alpine_tag("div",
      style = paste(
        "display: grid; grid-template-columns: 1fr 1fr 1fr;",
        "gap: 15px; margin-bottom: 20px;"
      ),

      # Category filter
      alpine_tag("div",
        alpine_tag("label","Filter by Category:"),
        alpine_tag("select",
          alpine_tag("option",value = "", "All"),
          alpine_tag("option",value = "A", "Category A"),
          alpine_tag("option",value = "B", "Category B"),
          alpine_tag("option",value = "C", "Category C")
        ) |>
          alpine_model("selectedCategory")
      ),

      # Region filter
      alpine_tag("div",
        alpine_tag("label","Filter by Region:"),
        alpine_tag("select",
          alpine_tag("option",value = "", "All"),
          alpine_tag("option",value = "North", "North"),
          alpine_tag("option",value = "South", "South"),
          alpine_tag("option",value = "East", "East"),
          alpine_tag("option",value = "West", "West")
        ) |>
          alpine_model("selectedRegion")
      ),

      # Search
      alpine_tag("div",
        alpine_tag("label","Search:"),
        alpine_tag("input",
          type = "text",
          placeholder = "Search..."
        ) |>
          alpine_model("searchText", .debounce = 300)
      )
    ),

    # Stats row
    alpine_tag("div",
      style = paste(
        "display: grid; grid-template-columns: 1fr 1fr 1fr;",
        "gap: 15px; margin-bottom: 20px;"
      ),

      alpine_tag("div",
        style = "background: #e3f2fd; padding: 15px; border-radius: 4px;",
        alpine_tag("strong","Total Records:"),
        alpine_tag("div",
          "x-text" = "getFilteredData().length"
        )
      ),

      alpine_tag("div",
        style = "background: #f3e5f5; padding: 15px; border-radius: 4px;",
        alpine_tag("strong","Avg Value:"),
        alpine_tag("div",
          "x-text" = "getFilteredData().length > 0 ? (getFilteredData().reduce((a,b) => a + b.value, 0) / getFilteredData().length).toFixed(2) : '0.00'"
        )
      ),

      alpine_tag("div",
        style = "background: #e8f5e9; padding: 15px; border-radius: 4px;",
        alpine_tag("strong","Selected Count:"),
        alpine_tag("div",
          "x-text" = "selectedKeys.length"
        )
      )
    ),

    # Data table
    alpine_crosstalk_table(
      shared_data,
      columns = c("id", "category", "region", "value", "date"),
      key_column = "id",
      selectable = TRUE,
      height = "500px"
    )
  ) |>
    alpine_data(
      selectedCategory = "",
      selectedRegion = "",
      searchText = "",
      selectedKeys = htmlwidgets::JS("[]"),
      filteredData = htmlwidgets::JS("data"),
      getFilteredData = htmlwidgets::JS("function() {
        const data = this.filteredData || [];
        return data.filter(item => {
          const categoryMatch = !this.selectedCategory || item.category === this.selectedCategory;
          const regionMatch = !this.selectedRegion || item.region === this.selectedRegion;
          const searchMatch = !this.searchText || 
            String(item.id).includes(this.searchText) ||
            String(item.category).includes(this.searchText) ||
            String(item.region).includes(this.searchText) ||
            String(item.value).toLowerCase().includes(this.searchText.toLowerCase());
          return categoryMatch && regionMatch && searchMatch;
        });
      }")
    )
}

# ============================================================================
# Export example functions for users to run
# ============================================================================

# Users would call these with:
# example_filter_table() |> htmltools::browsable()
# example_dashboard() |> htmltools::browsable()

# Save examples to HTML files
if (save_output) {
  htmltools::save_html(example_filter_table(), "09-crosstalk-filter-table.html")
  htmltools::save_html(example_dashboard(), "09-crosstalk-dashboard.html")
  cat("Saved crosstalk examples to current directory\n")
}

} else {
  # If crosstalk is not installed
  cat("Crosstalk examples require the 'crosstalk' package.\n")
  cat("Install with: install.packages('crosstalk')\n")
}
