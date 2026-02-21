# Alpine.js Examples

This directory contains runnable examples demonstrating real-world applications built with the alpinejs package.

## Running Examples

Each example is a standalone R script that can be run directly:

```bash
# From any directory
Rscript $(Rscript -e 'cat(system.file("examples", package="alpinejs"))')/01-todo-list.R

# Or if in the package source directory
Rscript inst/examples/01-todo-list.R
Rscript inst/examples/02-expense-tracker.R
Rscript inst/examples/03-settings-panel.R
Rscript inst/examples/04-inventory-dashboard.R
Rscript inst/examples/05-form-wizard.R
Rscript inst/examples/06-live-search.R
Rscript inst/examples/07-input-modifiers.R      # Specialized: all modifier patterns
Rscript inst/examples/08-plugins.R              # Specialized: plugin system
Rscript inst/examples/09-crosstalk-dashboard.R	# Specialized: Crosstalk integration
```

When run interactively, each script will generate an HTML file in your current working directory that you can open in your browser:

```bash
# View the generated files
open 01-todo-list.html          # macOS
xdg-open 01-todo-list.html      # Linux
start 01-todo-list.html         # Windows
```

**Note:** The output files are only generated when running examples interactively (not during automated builds or imports). You can browse and interact with the applications without needing to keep R running after the script completes.

## Examples Overview

### 01-todo-list.R
**A complete todo list application with persistence**

- Add, delete, and complete todos
- Filter by status (all, active, completed)
- Automatic localStorage persistence
- Computed properties for item counts

**Package Features Demonstrated:**
- `setup_persist()` plugin for localStorage integration
- `alpine_model()` for two-way data binding
- `alpine_on()` for event handling
- `alpine_bind(":class")` for dynamic CSS classes
- Computed properties with `htmlwidgets::JS()`

**Lines of Code:** 127

---

### 02-expense-tracker.R
**Track income and expenses with category-based calculations**

- Add transactions with category and amount
- View running balance and category totals
- Calculate expenses and income separately
- Real-time sum calculations

**Package Features Demonstrated:**
- Complex data structures (nested arrays)
- Grid layouts with CSS flexbox
- Computed properties for totals and balance
- Form validation with conditional buttons
- Radio buttons and select dropdowns
- `alpine_model(".number")` modifier for numeric inputs

**Lines of Code:** 205

---

### 03-settings-panel.R
**User preferences interface with tabbed sections**

- Switch between General, Notifications, and Privacy tabs
- Manage nested settings objects
- Save, reset, and delete preferences
- Status messages for user feedback

**Package Features Demonstrated:**
- Tabbed interface pattern with `alpine_bind()`
- Nested object management in Alpine data
- Toggle switches and checkboxes
- Multiple control types (text, toggle, checkbox, select)
- Conditional styling based on state
- Method calls for form operations

**Lines of Code:** 280+

---

### 04-inventory-dashboard.R
**Product inventory management with search and filtering**

- Real-time search with debouncing
- Filter by category and stock status
- Summary statistics (total products, low stock, inventory value)
- Dynamic table with responsive grid layout

**Package Features Demonstrated:**
- Debounced search input (250ms delay)
- `alpine_model(".debounce")` modifier
- `alpine_model(".trim")` modifier
- Complex data filtering logic
- Computed properties for counts and totals
- Grid-based layouts
- Conditional rendering of sections
- Color-coded stock status warnings

**Lines of Code:** 350+

---

### 05-form-wizard.R
**Multi-step form with progress tracking and validation**

- Three-step account creation wizard
- Step-by-step validation
- Progress bar visual indicator
- Summary review before submission
- Form reset after successful submission

**Package Features Demonstrated:**
- Conditional rendering with `:show`
- Step-by-step form state management
- Input validation and button disabling
- Progress bar with dynamic width
- Form reset and data persistence across steps
- Multi-section data organization
- Computed button states based on form completeness

**Lines of Code:** 350+

---

### 06-live-search.R
**Real-time search with filters and modal details**

- Debounced search input (300ms delay)
- Genre-based filtering with result counts
- Movie cards with ratings and genres
- Modal dialog for detailed movie information
- Search result highlighting and status

**Package Features Demonstrated:**
- Debounced input for responsive search
- Filter tabs with dynamic counts
- Grid layout with auto-fill responsive columns
- Modal with click-outside-to-close
- Hover effects with state tracking
- Computed properties for genre extraction
- `structuredClone()` for data isolation in modals
- Search across multiple fields

**Lines of Code:** 400+

---

### 07-input-modifiers.R
**Input modifiers: `.lazy`, `.debounce`, `.number`, `.trim`**

- Demonstrates all input modifier options
- Email validation with live feedback
- Number formatting and constraints
- List manipulation with dynamic updates

**Package Features Demonstrated:**
- `alpine_model(".lazy")` for blur-on-change behavior
- `alpine_model(".debounce")` for expensive operations
- `alpine_model(".number")` for automatic type conversion
- `alpine_model(".trim")` for whitespace handling
- Modifier combinations and ordering
- Input validation patterns
- Dynamic styling based on input state

**Purpose:** Reference for all input modifier options and combinations

**Lines of Code:** 300+

---

### 08-plugins.R
**Plugin system integration and custom Alpine extensions**

- Register and initialize Alpine plugins
- Persist data using the persist plugin for localStorage
- Create computed properties with plugin support
- Handle plugin lifecycle and initialization
- Multi-plugin composition

**Package Features Demonstrated:**
- `setup_persist()` plugin configuration
- Plugin registration and version management
- localStorage integration patterns
- Plugin state preservation
- Custom Alpine magic properties
- Computed properties with plugin support
- Reactive data persistence

**Purpose:** Reference for plugin system usage, especially localStorage persistence

**Lines of Code:** 250+

---

### 09-crosstalk-dashboard.R
**Cross-widget reactivity with Crosstalk integration**

- Synchronized table and charts via Crosstalk
- Row selection triggers updates in other widgets
- Filter changes propagate across all widgets
- SharedData for coordinated state management
- Multi-widget dashboards with reactive linking

**Package Features Demonstrated:**
- `alpine_crosstalk_table()` for selectable tables
- Crosstalk SharedData integration
- Selection event handling across widgets
- Filter synchronization patterns
- Dashboard composition with Crosstalk
- Group-based widget coordination
- Cross-widget reactive patterns

**Purpose:** Example of multi-widget dashboards with synchronized state

**Lines of Code:** 400+

---

## Documentation Layers

The alpinejs package provides documentation in three layers:

### 1. **Function Documentation** (Quick Reference)
Run `?function_name` in R to see examples:

- `?alpine_data` -- Component state and initialization
- `?alpine_model` -- Two-way data binding and modifiers
- `?alpine_on` -- Event handling patterns
- `?alpine_crosstalk_table` -- Crosstalk table creation

### 2. **Vignettes** (Guides)
Run `browseVignettes('alpinejs')` for detailed documentation:

- **Getting Started** -- Core concepts
- **Input Modifiers** -- `.lazy`, `.debounce`, `.number`, `.trim`
- **Components & Patterns** -- Reusable component patterns
- **Plugin Ecosystem** -- Plugin registration and lifecycle
- **Crosstalk Integration** -- Cross-widget reactivity
- **Performance & Best Practices** -- Optimization techniques

### 3. **Examples** (Complete Applications)
View patterns in `/examples/`:

- **01-06:** Full-featured application examples
- **07:** Input modifier patterns
- **08:** Plugin system example
- **09:** Crosstalk dashboard example

---

## Recommended Learning Path

### Core Learning Sequence (01-06)

1. **Start with 01-todo-list.R**
   - Foundational example showcasing basic state, methods, and plugins
   - Good introduction to persistent data with Alpine

2. **Move to 02-expense-tracker.R**
   - Learn complex data structures and computed properties
   - Understand form validation patterns

3. **Then explore 03-settings-panel.R**
   - Tabbed interface patterns
   - Managing nested settings objects
   - Multi-control type forms

4. **Study 04-inventory-dashboard.R**
   - Debouncing and input modifiers in action
   - Real-time filtering of datasets
   - Summary statistics and computed values

5. **Review 05-form-wizard.R**
   - Multi-step form patterns
   - Progress tracking
   - Form reset workflows

6. **06-live-search.R** -- Advanced filtering with genre extraction
   - Advanced filtering with genre extraction
   - Modal dialogs
   - Responsive grid layouts
   - Complex computed properties

### Specialized References (07-09)

After completing the core sequence, explore these specialized references as needed:

- **07-input-modifiers.R** -- Deep dive into `alpine_model()` modifiers
  - Use when: Building form inputs with specific behaviors
  - See also: Function docs with `?alpine_model`, vignette on Input Modifiers

- **08-plugins.R** -- Plugin system patterns
  - Use when: Integrating plugins like persist, mask, or focus
  - See also: Function docs with `?setup_persist`, vignette on Plugin Ecosystem

- **09-crosstalk-dashboard.R** -- Multi-widget coordination
  - Use when: Building dashboards with synchronized widgets
  - See also: Function docs with `?alpine_crosstalk_table`, vignette on Crosstalk Integration

## Common Patterns Across Examples

### Data Initialization
All examples use `alpine_data()` with initial state and computed properties:

```r
app |>
  alpine_data(
    searchTerm = "",
    items = htmlwidgets::JS("[...]"),
    filteredItems = htmlwidgets::JS("get filteredItems() { ... }")
  )
```

### Method Definition
Methods are defined using `htmlwidgets::JS()` to preserve function context:

```r
addItem = htmlwidgets::JS("
  function(item) {
    this.items.push(item);
  }
")
```

### Event Handling
Events are bound using `alpine_on()`:

```r
button("Add") |> alpine_on("click", "addItem(newItem)")
```

### Computed Properties
Properties that derive from data are computed with getters:

```r
filteredItems = htmlwidgets::JS("
  get filteredItems() {
    return this.items.filter(i => i.name.includes(this.search));
  }
")
```

### Input Modifiers
Use modifiers for input processing:

```r
input() |> alpine_model("value", .debounce = 300, .trim = TRUE)
```

## Styling Approach

All examples use inline styles with modern CSS:

- **Flexbox** for simple layouts
- **CSS Grid** for complex arrangements
- **CSS variables** for spacing and colors
- **Transitions** for smooth interactions
- **Custom hover states** for interactivity

This approach keeps examples self-contained and easily customizable.

## Extending the Examples

Each example can be extended by:

1. Adding more x-data properties
2. Creating additional computed properties
3. Implementing new methods
4. Adding more complex validation
5. Integrating with external data sources
6. Using the `crosstalk` package for cross-widget reactivity

## Notes

- Examples prioritize clarity for learning purposes
- All examples are runnable and functional
- Examples use realistic data to demonstrate patterns
- Comments explain complex functionality
- Styling follows responsive design principles

---

For more information, see the project repository.
