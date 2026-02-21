#!/usr/bin/env Rscript
#
# Build documentation site with litedown
# Intelligent caching: only regenerates files if source changed since last build
#
# Usage: Rscript build_docs.R [--clean]
#

library(litedown)
source("_site_config.R")
source("_reference_config.R")

# Configure litedown rendering defaults
# Individual chunks can override these with their own options in chunk headers
litedown::reactor(list(
  engine = "R",
  eval = TRUE,
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  collapse = TRUE,
  comment = "#> "
))

# Parse arguments
args <- commandArgs(trailingOnly = TRUE)
clean_build <- "--clean" %in% args

cat("alpinejs Documentation Builder\n")
cat("===============================\n\n")

# Setup directories
dir.create("docs", showWarnings = FALSE)
dir.create("docs/guides", showWarnings = FALSE)
dir.create("docs/examples", showWarnings = FALSE)
dir.create("docs/assets", showWarnings = FALSE)

# Cache file to track build times
cache_file <- ".docs_build_cache"
build_cache <- if (file.exists(cache_file) && !clean_build) {
  readRDS(cache_file)
} else {
  list()
}

# Helper: Check if file needs rebuild
needs_build <- function(src_file, out_file) {
  if (!file.exists(out_file)) return(TRUE)
  src_mtime <- file.info(src_file)$mtime
  cached_mtime <- build_cache[[src_file]]
  if (is.null(cached_mtime)) return(TRUE)
  src_mtime > cached_mtime
}

# Helper: Calculate relative path prefix for navigation
calculate_rel_prefix <- function(output_file) {
  # output_file is like "docs/index.html" or "docs/guides/getting-started.html"
  # Count directory depth within docs
  rel_path <- sub("^docs/", "", output_file)
  depth <- length(strsplit(rel_path, "/")[[1]]) - 1  # -1 because last part is filename
  if (depth == 0) {
    return("")  # Root level
  } else {
    return(strrep("../", depth))
  }
}

# Helper: Read and render Rmd file
render_file <- function(input_file, output_file, title = NULL, section = "guides") {
  tryCatch({
    cat(sprintf("  Rendering: %s ... ", basename(input_file)))

    # Read file
    content <- readLines(input_file, warn = FALSE)

    # Extract title from YAML
    if (is.null(title)) {
      yaml_match <- grep("^title:", content)
      if (length(yaml_match) > 0) {
        title_line <- content[yaml_match[1]]
        title <- sub('^title:\\s*["\']?([^"\']*)["\']?\\s*$', "\\1", title_line)
      } else {
        title <- tools::file_path_sans_ext(basename(input_file))
      }
    }

    # Process Rmd directly with litedown::fuse() which executes R code and converts to HTML
    # fuse() returns a complete HTML document, so we extract just the body content
    full_html <- litedown::fuse(input_file, output = NA, quiet = TRUE)
    
    # Extract body content using reliable string splitting
    # Split on opening body tag (handles attributes)
    body_parts <- strsplit(full_html, "<body[^>]*>", perl = TRUE)[[1]]
    
    if (length(body_parts) > 1) {
      # Take everything after opening body tag
      content_with_footer <- body_parts[2]
      # Split on closing body tag and take everything up to it
      body_content <- strsplit(content_with_footer, "</body>", perl = TRUE)[[1]][1]
      html_body <- body_content
    } else {
      # Fallback if no body tags found
      html_body <- full_html
    }
    
    # Unescape ONLY non-quote HTML entities
    # Quote entities (&quot;, &#39;, &#34;, &#x22;, &#x27;) MUST stay
    # escaped in HTML attributes to preserve valid HTML!
    # The browser automatically decodes all entities, so we only unescape
    # the special ones that break attributes (quotes) - leave them escaped
    html_body <- gsub("&lt;", "<", html_body, fixed = TRUE)
    html_body <- gsub("&gt;", ">", html_body, fixed = TRUE)

    # Unescape numeric HTML entities EXCEPT quote codes (34, 39, 0x22, 0x27)
    # Use gsub with negative lookbehind/lookahead (not directly supported in R)
    # So use a replacement strategy that excludes quotes
    temp <- html_body
    pos <- 1
    while (TRUE) {
      match <- regexpr("&#([0-9]+);", substr(temp, pos, nchar(temp)), perl = TRUE)
      if (match[1] == -1) break
      match_start <- pos + match[1] - 1
      match_end <- match_start + attr(match, "match.length")[1] - 1
      match_str <- substr(html_body, match_start, match_end)
      code_point <- as.integer(sub("&#([0-9]+);", "\\1", match_str))

      if (!(code_point %in% c(34, 39))) {  # Skip quote entities
        replacement <- tryCatch(intToUtf8(code_point), error = function(e) match_str)
        html_body <- sub(match_str, replacement, html_body, fixed = TRUE)
      }
      pos <- match_end + 1
    }

    # Same for hex entities
    temp <- html_body
    pos <- 1
    while (TRUE) {
      match <- regexpr("&#x([0-9a-fA-F]+);", substr(temp, pos, nchar(temp)),
                       perl = TRUE, ignore.case = TRUE)
      if (match[1] == -1) break
      match_start <- pos + match[1] - 1
      match_end <- match_start + attr(match, "match.length")[1] - 1
      match_str <- substr(html_body, match_start, match_end)
      hex_str <- sub("&#x([0-9a-fA-F]+);", "\\1", match_str, ignore.case = TRUE)
      code_point <- strtoi(paste0("0x", hex_str), base = 16)

      if (!(code_point %in% c(34, 39))) {  # Skip quote entities
        replacement <- tryCatch(intToUtf8(code_point), error = function(e) match_str)
        html_body <- sub(match_str, replacement, html_body, fixed = TRUE)
      }
      pos <- match_end + 1
    }

    # Build breadcrumb navigation for individual pages (not index pages)
    breadcrumb <- ""
    if (section %in% c("guides", "examples")) {
      # Generate breadcrumb: Home > Guides > Page Title
      section_name <- if (section == "guides") "Guides" else "Examples"
      section_path <- if (section == "guides") "guides/" else "examples/"

      # Calculate relative paths based on output file depth
      rel_prefix <- calculate_rel_prefix(output_file)
      link_style <- "style=\"color: #0000ff; text-decoration: underline;\""
      breadcrumb <- sprintf(
        paste(
          '<nav style="margin-bottom: 1.5rem; font-size: 0.9em;">',
          '<a href="%sindex.html" %s>Home</a> &gt;',
          '<a href="%s%sindex.html" %s>%s</a> &gt;',
          '<span style="color: #666;">%s</span>',
          '</nav>',
          sep = "\n      "
        ),
        rel_prefix,
        link_style,
        rel_prefix,
        section_path,
        link_style,
        section_name,
        title
      )
    }

    # Add breadcrumb to end of body content
    html_body <- paste0(html_body, breadcrumb)

    # Wrap in template
    template <- readLines("_site_template.html")
    page <- paste(template, collapse = "\n")

    # Calculate and set relative path prefix
    rel_prefix <- calculate_rel_prefix(output_file)

    page <- gsub("{{title}}", title, page, fixed = TRUE)
    page <- gsub("{{content}}", html_body, page, fixed = TRUE)
    page <- gsub("{{section}}", section, page, fixed = TRUE)
    page <- gsub("{{REL_PREFIX}}", rel_prefix, page, fixed = TRUE)

    # Write output
    writeLines(page, output_file)

    # Update cache
    build_cache[[input_file]] <<- file.info(input_file)$mtime

    cat("✓\n")
    TRUE
  }, error = function(e) {
    cat("✗ ERROR:", conditionMessage(e), "\n")
    FALSE
  })
}

# === BUILD GUIDES ===

cat("\n[1/3] Building Guides\n")
cat("---------------------\n")

guides_built <- 0
for (guide_name in names(site_guides)) {
  rmd_file <- file.path("vignettes", paste0(site_guides[[guide_name]], ".Rmd"))
  html_file <- file.path("docs/guides", paste0(site_guides[[guide_name]], ".html"))

  if (file.exists(rmd_file)) {
    if (needs_build(rmd_file, html_file) || clean_build) {
      if (render_file(rmd_file, html_file, guide_name, section = "guides")) {
        guides_built <- guides_built + 1
      }
    } else {
      cat(sprintf("  Skipping: %s (unchanged)\n", basename(rmd_file)))
    }
  } else {
    cat(sprintf("  WARNING: %s not found\n", rmd_file))
  }
}

cat(sprintf("\nGuides: %d/%d rendered\n", guides_built, length(site_guides)))

# === BUILD EXAMPLES ===

cat("\n[2/3] Building Examples\n")
cat("------------------------\n")

examples_built <- 0
for (example_name in names(site_examples)) {
  rmd_file <- file.path("vignettes/examples", paste0(site_examples[[example_name]], ".Rmd"))
  html_file <- file.path("docs/examples", paste0(site_examples[[example_name]], ".html"))

  if (file.exists(rmd_file)) {
    if (needs_build(rmd_file, html_file) || clean_build) {
      if (render_file(rmd_file, html_file, example_name, section = "examples")) {
        examples_built <- examples_built + 1
      }
    } else {
      cat(sprintf("  Skipping: %s (unchanged)\n", basename(rmd_file)))
    }
  } else {
    cat(sprintf("  WARNING: %s not found\n", rmd_file))
  }
}

cat(sprintf("\nExamples: %d/%d rendered\n", examples_built, length(site_examples)))

# === BUILD INDEX PAGES ===

cat("\n[2b/3] Building Index Pages\n")
cat("----------------------------\n")

# Helper: Generate index page HTML
generate_index_page <- function(title, section, items, item_names) {
  # Build list of links
  links_html <- paste(
    sprintf(
      '    <li><a href="%s.html">%s</a></li>',
      items,
      item_names
    ),
    collapse = "\n"
  )

  # Build page HTML
  # Handle singular/plural for descriptions
  article <- if (section == "guide") "a" else "an"

  body_content <- sprintf('
    <h1>%s</h1>
    <p>Choose %s %s to explore Alpine.js components and patterns:</p>
    <ul>
%s
    </ul>
  ', title, article, section, links_html)

  body_content
}

# Generate Guides index
guides_body <- generate_index_page(
  "Guides",
  "guide",
  site_guides,
  names(site_guides)
)

guides_index_file <- "docs/guides/index.html"
template <- readLines("_site_template.html")
page <- paste(template, collapse = "\n")
rel_prefix <- "../"  # One level up from guides/
page <- gsub("{{title}}", "Guides | alpinejs", page, fixed = TRUE)
page <- gsub("{{content}}", guides_body, page, fixed = TRUE)
page <- gsub("{{section}}", "guides", page, fixed = TRUE)
page <- gsub("{{REL_PREFIX}}", rel_prefix, page, fixed = TRUE)
writeLines(page, guides_index_file)
cat("  Guides index ... ✓\n")

# Generate Examples index
examples_body <- generate_index_page(
  "Examples",
  "example",
  site_examples,
  names(site_examples)
)

examples_index_file <- "docs/examples/index.html"
page <- paste(template, collapse = "\n")
rel_prefix <- "../"  # One level up from examples/
page <- gsub("{{title}}", "Examples | alpinejs", page, fixed = TRUE)
page <- gsub("{{content}}", examples_body, page, fixed = TRUE)
page <- gsub("{{section}}", "examples", page, fixed = TRUE)
page <- gsub("{{REL_PREFIX}}", rel_prefix, page, fixed = TRUE)
writeLines(page, examples_index_file)
cat("  Examples index ... ✓\n")

cat("\nIndex pages: guides and examples created\n")

cat("\n[3/3] Building Root Pages\n")
cat("---------------------------\n")

# README
readme_file <- "README.Rmd"
if (file.exists(readme_file)) {
  if (needs_build(readme_file, "docs/index.html") || clean_build) {
    if (render_file(readme_file, "docs/index.html", "alpinejs", section = "home")) {
      cat("Root pages: index built\n")
    }
  } else {
    cat("  Skipping: README.Rmd (unchanged)\n")
  }
}

# === BUILD REFERENCE PAGES ===

cat("\n[3b/3] Building Reference Pages\n")
cat("----------------------------\n")

# Ensure reference directory exists
dir.create("docs/reference", showWarnings = FALSE)

# Build reference pages from .Rd files using tools package
rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
rd_files <- grep("^man/alpine_|^man/add_", rd_files, value = TRUE)

ref_pages_built <- 0
for (rd_file in sort(rd_files)) {
  func_name <- tools::file_path_sans_ext(basename(rd_file))
  output_file <- file.path("docs/reference", paste0(func_name, ".html"))

  tryCatch({
    cat(sprintf("  %s ... ", func_name))

    # Convert .Rd to HTML using tools::Rd2HTML
    html_content <- paste(capture.output(tools::Rd2HTML(rd_file)), collapse = "\n")

    # Extract just the body content (between <body> and </body>)
    body_match <- regexpr("<body[^>]*>", html_content)
    if (body_match[1] > 0) {
      body_start <- body_match[1] + attr(body_match, "match.length")[1]
      body_end <- regexpr("</body>", html_content)[1] - 1
      html_content <- substr(html_content, body_start, body_end)
    }

    # Clean up extra whitespace
    html_content <- gsub("^[[:space:]]+|[[:space:]]+$", "", html_content)

    # Calculate relative paths (reference files are 1 level deep in reference/)
    rel_up <- "../"

    # Build breadcrumb navigation
    breadcrumb <- sprintf(
      paste(
        '<nav style="margin-bottom: 1.5rem; font-size: 0.9em;">',
        '<a href="%sindex.html" ',
        'style="color: #0000ff; text-decoration: underline;">',
        'Home</a> &gt;',
        '<a href="index.html" ',
        'style="color: #0000ff; text-decoration: underline;">',
        'Reference</a> &gt;',
        '<span style="color: #666;">%s</span>',
        '</nav>',
        sep = "\n      "
      ),
      rel_up,
      func_name
    )

    # Combine breadcrumb with content
    html_body <- paste0(breadcrumb, html_content)

    # Wrap in template
    template <- readLines("_site_template.html")
    page <- paste(template, collapse = "\n")
    page <- gsub("{{title}}", paste0(func_name, " | alpinejs"), page, fixed = TRUE)
    page <- gsub("{{content}}", html_body, page, fixed = TRUE)
    page <- gsub("{{section}}", "reference", page, fixed = TRUE)
    page <- gsub("{{REL_PREFIX}}", rel_up, page, fixed = TRUE)

    # Write output
    writeLines(page, output_file)

    cat("✓\n")
    ref_pages_built <- ref_pages_built + 1
  }, error = function(e) {
    cat("✗ -", conditionMessage(e), "\n")
  })
}

cat(sprintf("Reference: %d pages built\n", ref_pages_built))

# Helper: Extract class/category from configuration or .Rd file
extract_class_from_rd <- function(func_name, rd_file) {
  # First, check if function is defined in reference_classes config
  if (func_name %in% names(reference_classes)) {
    return(reference_classes[[func_name]])
  }

  # Otherwise, look for \keyword{class:ClassName} pattern in .Rd file
  content <- readLines(rd_file, warn = FALSE)
  class_lines <- grep("\\\\keyword.*class:", content, value = TRUE)

  if (length(class_lines) > 0) {
    # Extract the class name from first match
    class_pattern <- gsub(".*\\\\keyword\\{class:([^}]*)\\}.*", "\\1", class_lines[1])
    if (class_pattern != class_lines[1]) {
      return(class_pattern)
    }
  }

  return("Other")  # Default class if none specified
}

# Generate Reference index with class grouping
ref_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
ref_files <- grep("^man/alpine_|^man/add_", ref_files, value = TRUE)
ref_names <- tools::file_path_sans_ext(basename(ref_files))

# Extract classes for each function
ref_classes <- setNames(rep(NA, length(ref_names)), ref_names)
for (i in seq_along(ref_files)) {
  ref_classes[ref_names[i]] <- extract_class_from_rd(ref_names[i], ref_files[i])
}

# Group functions by class
class_groups <- split(names(ref_classes), ref_classes[names(ref_classes)])
class_groups <- class_groups[order(names(class_groups))]  # Sort by class name

# Build HTML for grouped reference links
grouped_html <- ""
for (class_name in names(class_groups)) {
  func_names <- sort(class_groups[[class_name]])

  # Add class header
  grouped_html <- paste0(
    grouped_html,
    sprintf("    <h3>%s</h3>\n    <ul>\n", class_name)
  )

  # Add function links
  for (func_name in func_names) {
    grouped_html <- paste0(
      grouped_html,
      sprintf('    <li><a href="%s.html">%s</a></li>\n', func_name, func_name)
    )
  }

  grouped_html <- paste0(grouped_html, "    </ul>\n")
}

# Build reference index body
ref_body <- sprintf(
  '
    <h1>Reference</h1>
    <p>Complete function reference for the alpineR package. Functions are organized by category:</p>
%s
  ',
  grouped_html
)

# Write reference index
ref_index_file <- "docs/reference/index.html"
template <- readLines("_site_template.html")
page <- paste(template, collapse = "\n")
rel_prefix <- "../"
page <- gsub("{{title}}", "Reference | alpinejs", page, fixed = TRUE)
page <- gsub("{{content}}", ref_body, page, fixed = TRUE)
page <- gsub("{{section}}", "reference", page, fixed = TRUE)
page <- gsub("{{REL_PREFIX}}", rel_prefix, page, fixed = TRUE)
writeLines(page, ref_index_file)
cat("  Reference index ... ✓\n")

cat("\nIndex pages: guides, examples, and reference created\n")

# === COPY ASSETS ===

cat("\nCopying assets...\n")
assets_src <- "assets"
if (dir.exists(assets_src)) {
  file.copy(
    dir(assets_src, full.names = TRUE),
    "docs/assets/",
    recursive = TRUE,
    overwrite = TRUE
  )
  cat("  Assets copied\n")
}

# === SAVE CACHE ===

saveRDS(build_cache, cache_file)

# === SUMMARY ===

cat("\n===============================\n")
cat("Build Complete!\n")
cat(sprintf("Total files rendered: %d\n", guides_built + examples_built + 1))
cat("\nOutput: docs/\n")
cat("\nTo preview locally:\n")
cat("  python3 -m http.server 8000 -d docs/\n")
cat("  Then visit http://localhost:8000\n")
