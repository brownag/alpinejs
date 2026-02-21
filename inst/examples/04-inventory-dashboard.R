#' Product Inventory Dashboard
#'
#' A dashboard showing product inventory with search, filters, and sorting.
#' Demonstrates debouncing, modifiers, and dynamic table rendering.
#'
#' Run: Rscript inst/examples/04-inventory-dashboard.R

library(alpinejs)
library(htmltools)

save_output <- interactive()

# Define the data object as JavaScript in global scope.
# In production, this would typically be in a separate .js file and loaded
# via <script src="..."></script>. For examples, we inline it for coherence.
inventory_script <- alpine_tag("script", htmltools::HTML("
  window.inventoryApp = {
    searchTerm: '',
    selectedCategory: '',
    stockFilter: 'all',
    nextId: 11,
    newProduct: { name: '', category: 'Electronics', quantity: 0, price: 0 },
    products: [
      { id: 1, name: 'Laptop', category: 'Electronics', quantity: 5, price: 999.99 },
      { id: 2, name: 'Mouse', category: 'Electronics', quantity: 45, price: 19.99 },
      { id: 3, name: 'Desk Chair', category: 'Furniture', quantity: 8, price: 299.99 },
      { id: 4, name: 'Monitor', category: 'Electronics', quantity: 12, price: 349.99 },
      { id: 5, name: 'Keyboard', category: 'Electronics', quantity: 0, price: 79.99 },
      { id: 6, name: 'Desk Lamp', category: 'Furniture', quantity: 25, price: 45.99 },
      { id: 7, name: 'USB Cable', category: 'Accessories', quantity: 150, price: 4.99 },
      { id: 8, name: 'HDMI Cable', category: 'Accessories', quantity: 3, price: 8.99 },
      { id: 9, name: 'Standing Desk', category: 'Furniture', quantity: 2, price: 599.99 },
      { id: 10, name: 'Webcam', category: 'Electronics', quantity: 7, price: 89.99 }
    ],
    categories: function() {
      const cats = new Set(this.products.map(p => p.category));
      return Array.from(cats).sort();
    },
    lowStockCount: function() {
      return this.products.filter(p => p.quantity > 0 && p.quantity <= 10).length;
    },
    totalValue: function() {
      return this.products.reduce((sum, p) => sum + (p.quantity * p.price), 0);
    },
    filteredProducts: function() {
      return this.products.filter(p => {
        if (this.searchTerm && !p.name.toLowerCase().includes(this.searchTerm.toLowerCase())) return false;
        if (this.selectedCategory && p.category !== this.selectedCategory) return false;
        if (this.stockFilter === 'low' && (p.quantity <= 0 || p.quantity > 10)) return false;
        if (this.stockFilter === 'out' && p.quantity > 0) return false;
        return true;
      });
    },
    addProduct: function() {
      if (!this.newProduct.name || !this.newProduct.category || this.newProduct.price <= 0) return;
      this.products.push({
        id: this.nextId++,
        name: this.newProduct.name,
        category: this.newProduct.category,
        quantity: parseInt(this.newProduct.quantity) || 0,
        price: parseFloat(this.newProduct.price)
      });
      this.newProduct = { name: '', category: 'Electronics', quantity: 0, price: 0 };
    },
    updateQuantity: function(id, newQty) {
      if (newQty < 0) return;
      const product = this.products.find(p => p.id === id);
      if (product) product.quantity = newQty;
    },
    deleteProduct: function(id) {
      this.products = this.products.filter(p => p.id !== id);
    }
  };
"))

# Build app with alpineR - uses alpine_data_js() for global object
app <- alpine_tag("div",
  class = "dashboard-container",
  style = "max-width: 1000px; margin: 40px auto; font-family: sans-serif;",

  # Header
  alpine_tag("h1","Product Inventory", style = "color: #333; margin-bottom: 10px;"),
  alpine_tag("p","Manage your product inventory with real-time search and filters", style = "color: #666; margin-bottom: 30px;"),

  # Stats row
  alpine_tag("div",
    style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 15px; margin-bottom: 30px;",

    alpine_stat_card("Total Products", "products.length", bg_color = "#f0f8ff", text_color = "#0066cc"),
    alpine_stat_card("Low Stock", "lowStockCount()", bg_color = "#fffdf0", text_color = "#ff8800"),
    alpine_stat_card("Total Value", "'$' + totalValue().toFixed(2)", bg_color = "#f0fff0", text_color = "#00aa00"),
    alpine_stat_card("Categories", "categories().length", bg_color = "#f5f5f5", text_color = "#666")
  ),

    # Filters row
    alpine_tag("div",
      style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 15px; margin-bottom: 20px;",

      # Search
      alpine_tag("div",
        alpine_tag("label","Search", style = "display: block; margin-bottom: 5px; color: #666; font-size: 14px;"),
        alpine_tag("input",
          type = "text",
          placeholder = "Search by product name...",
          style = "width: 100%; padding: 10px; border: 1px solid #ddd; border-radius: 4px;"
        ) |>
          alpine_model("searchTerm", .debounce = 250, .trim = TRUE)
      ),

      # Category filter
      alpine_tag("div",
        alpine_tag("label","Category", style = "display: block; margin-bottom: 5px; color: #666; font-size: 14px;"),
        alpine_tag("select",
          alpine_tag("option",value = "", "All Categories"),
          alpine_for(
            alpine_tag("option",style = "width: 100%; padding: 10px; border: 1px solid #ddd; border-radius: 4px;") |>
              alpine_bind("value", "category") |>
              alpine_text("category"),
            "category in categories()"
          ),
          style = "width: 100%; padding: 10px; border: 1px solid #ddd; border-radius: 4px;"
        ) |>
          alpine_model("selectedCategory")
      ),

      # Stock filter
      alpine_tag("div",
        alpine_tag("label","Stock Status", style = "display: block; margin-bottom: 5px; color: #666; font-size: 14px;"),
        alpine_filter_buttons(
          c("All Items" = "all", "Low Stock Only" = "low", "Out of Stock" = "out"),
          "stockFilter"
        )
      )
    ),

    # Add Product Form
    alpine_tag("div",
      style = "background: #f0f8ff; padding: 20px; border-radius: 8px; margin-bottom: 30px; border: 2px solid #007bff;",
      alpine_tag("h3","Add New Product", style = "margin-top: 0; color: #333;"),
      alpine_tag("div",
        style = "display: grid; grid-template-columns: 2fr 1fr 1fr 1fr auto; gap: 10px;",
        
        alpine_tag("input",
          type = "text",
          placeholder = "Product name",
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 4px;"
        ) |>
          alpine_model("newProduct.name", .trim = TRUE),

        alpine_tag("select",
          alpine_for(
            alpine_tag("option") |>
              alpine_text("category") |>
              alpine_bind("value", "category"),
            "category in categories()"
          ),
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 4px;"
        ) |>
          alpine_model("newProduct.category"),

        alpine_tag("input",
          type = "number",
          placeholder = "Qty",
          min = "0",
          step = "1",
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 4px;"
        ) |>
          alpine_model("newProduct.quantity", .number = TRUE),

        alpine_tag("input",
          type = "number",
          placeholder = "Price",
          min = "0",
          step = "0.01",
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 4px;"
        ) |>
          alpine_model("newProduct.price", .number = TRUE),

        alpine_tag("button",
          "Add",
          style = "background: #28a745; color: white; border: none; padding: 10px 20px; border-radius: 4px; cursor: pointer; font-weight: bold;"
        ) |>
          alpine_bind("disabled", "!newProduct.name || !newProduct.category || newProduct.quantity < 0 || newProduct.price <= 0") |>
          alpine_on("click", "addProduct()")
      )
    ),

    # Table
    alpine_tag("div",
      style = "background: white; border-radius: 8px; overflow: hidden; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",

      # Header row
      alpine_tag("div",
        style = "display: grid; grid-template-columns: 2fr 1fr 1fr 1fr 1fr 120px; gap: 15px; padding: 15px; background: #f9f9f9; border-bottom: 2px solid #eee; font-weight: bold; color: #333;",
        "Product",
        "Category",
        "Quantity",
        "Price",
        "Value",
        "Actions"
      ),

      # Product rows
      alpine_for(
        alpine_tag("div",
          style = "display: grid; grid-template-columns: 2fr 1fr 1fr 1fr 1fr 120px; gap: 15px; padding: 15px; border-bottom: 1px solid #eee; align-items: center;",

          # Product name
          alpine_tag("div") |>
            alpine_text("product.name") |>
            alpine_bind("style", "product.quantity <= 10 ? 'color: #ff8800; font-weight: bold;' : 'color: #333;'"),

          # Category badge
          alpine_tag("div",
            style = "padding: 4px 8px; background: #e3f2fd; color: #1976d2; border-radius: 4px; font-size: 12px; text-align: center;"
          ) |>
            alpine_text("product.category"),

          # Quantity
          alpine_tag("div") |>
            alpine_text("product.quantity") |>
            alpine_bind("style", "product.quantity <= 5 ? 'color: #dc3545; font-weight: bold;' : product.quantity <= 10 ? 'color: #ff8800;' : 'color: #333;'"),

          # Price
          alpine_tag("div",
            style = "text-align: right;"
          ) |>
            alpine_text("'$' + product.price.toFixed(2)"),

          # Value
          alpine_tag("div",
            style = "text-align: right; font-weight: bold;"
          ) |>
            alpine_text("'$' + (product.quantity * product.price).toFixed(2)"),

          # Actions
          alpine_tag("div",
            style = "display: flex; gap: 5px;",
            alpine_tag("button",
              "-",
              style = "background: #ff8800; color: white; border: none; padding: 4px 8px; border-radius: 3px; cursor: pointer; font-size: 12px; font-weight: bold;"
            ) |>
              alpine_on("click", "updateQuantity(product.id, product.quantity - 1)"),

            alpine_tag("button",
              "+",
              style = "background: #28a745; color: white; border: none; padding: 4px 8px; border-radius: 3px; cursor: pointer; font-size: 12px; font-weight: bold;"
            ) |>
              alpine_on("click", "updateQuantity(product.id, product.quantity + 1)"),

            alpine_tag("button",
              "Del",
              style = "background: #dc3545; color: white; border: none; padding: 4px 8px; border-radius: 3px; cursor: pointer; font-size: 12px;"
            ) |>
              alpine_on("click", "deleteProduct(product.id)")
          )
        ),
        "product in filteredProducts()",
        key = "product.id"
      ),

      # Empty state
      alpine_tag("div",
        style = "padding: 40px; text-align: center; color: #999;"
      ) |>
        alpine_show("filteredProducts().length === 0") |>
        alpine_text("No products found matching your criteria")
    )
) |>
  alpine_data_js("window.inventoryApp") |>
  htmltools::browsable()

# Wrap with script tag for global object
app <- htmltools::tagList(
  inventory_script,
  app
)

if (save_output) {
  htmltools::save_html(app, file = "04-inventory-dashboard.html")
  cat("Saved to: 04-inventory-dashboard.html\n")
}
