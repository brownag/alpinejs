#' @keywords internal
#' Internal utility: Check if an object is a JS() wrapped expression
#'
#' Detects objects created by htmlwidgets::JS() or similar that should
#' be injected as raw JavaScript rather than serialized as JSON.
#'
#' @param x An R object to test.
#'
#' @return Logical. `TRUE` if the object is a JS expression, `FALSE` otherwise.
#'
#' @noRd
is_js_expression <- function(x) {
  inherits(x, "JS_EVAL") || inherits(x, "html")
}

#' @keywords internal
#' Internal utility: Convert R data to Alpine.js compatible JSON
#'
#' Intelligently serializes R objects to JSON suitable for Alpine.js x-data
#' initialization. Handles type conversions (factors to characters, dates to
#' ISO8601 strings) and preserves raw JavaScript expressions marked with JS().
#'
#' @param ... Named arguments or a list to serialize.
#'
#' @return A character string containing valid JSON suitable for Alpine.js
#'   x-data attributes.
#'
#' @details
#' **Type Conversion Rules:**
#' - Logical (TRUE/FALSE) -> JSON booleans (true/false)
#' - Factors -> character vectors
#' - Dates/POSIXct -> ISO 8601 strings (format: YYYY-MM-DD or YYYY-MM-DDTHH:MM:SSTZ)
#' - NA values -> JSON null
#' - data.frame -> array of objects (rows become objects)
#' - Vectors -> JSON arrays
#' - Lists -> JSON objects (if named) or arrays (if unnamed)
#' - JS() expressions -> injected as raw JavaScript (not quoted)
#'
#' @noRd
serialize_r_to_json <- function(...) {
  args <- list(...)

  # If no arguments, return empty object
  if (length(args) == 0) {
    return("{}")
  }

  # Split into JS and non-JS components
  is_js <- vapply(args, is_js_expression, logical(1))
  json_parts <- args[!is_js]
  js_parts <- args[is_js]

  # Serialize non-JS components
  if (length(json_parts) > 0) {
    # Convert special types before serializing
    json_parts <- lapply(json_parts, convert_r_type_for_json)

    # Use jsonlite with auto_unbox to get single values as scalars not arrays
    json_str <- jsonlite::toJSON(
      json_parts,
      auto_unbox = TRUE,
      na = "null",
      digits = NA
    )

    # Remove outer brackets if this is an array (should be object)
    if (grepl("^\\[", json_str) && grepl("\\]$", json_str)) {
      json_str <- substr(json_str, 2, nchar(json_str) - 1)
    }
  } else {
    json_str <- ""
  }

  # Merge JS parts back in if present
  if (length(js_parts) > 0) {
    # Extract raw JS strings and minify them (remove newlines for CSP compatibility)
    js_values <- vapply(js_parts, function(x) {
      if (inherits(x, "JS_EVAL")) {
        js_str <- as.character(x)
      } else if (inherits(x, "html")) {
        js_str <- as.character(x)
      } else {
        js_str <- ""
      }
      
      # Minify: collapse whitespace and remove newlines
      # This is essential for CSP-compliant Alpine.js which can't parse multiline expressions
      js_str <- gsub("\n", " ", js_str, fixed = TRUE)  # Replace newlines with spaces
      js_str <- gsub("\r", " ", js_str, fixed = TRUE)  # Replace carriage returns with spaces
      js_str <- gsub("  +", " ", js_str)                # Collapse multiple spaces into one
      js_str <- trimws(js_str)                         # Trim leading/trailing whitespace
      
      js_str
    }, character(1))

    # Extract names
    js_names <- names(js_parts)

    # Build JS key-value pairs
    js_pairs <- character(length(js_values))
    for (i in seq_along(js_values)) {
      js_pairs[i] <- sprintf("\"%s\":%s", js_names[i], js_values[i])
    }

    # Merge JSON and JS parts
    if (nchar(json_str) > 0) {
      # Remove trailing } if present
      json_str <- sub("}\\s*$", "", json_str)
      json_str <- paste0(json_str, ",", paste(js_pairs, collapse = ","), "}")
    } else {
      json_str <- paste0("{", paste(js_pairs, collapse = ","), "}")
    }
  } else {
    # Just ensure object braces
    if (!nchar(json_str)) {
      json_str <- "{}"
    } else if (!starts_with_brace(json_str)) {
      json_str <- paste0("{", json_str, "}")
    }
  }

  # Minify the final JSON string: remove all newlines and collapse whitespace
  # HTMLtools attributes cannot contain literal newlines, and they break HTML attribute parsing
  json_str <- gsub("\n", " ", json_str, fixed = TRUE)
  json_str <- gsub("\r", " ", json_str, fixed = TRUE)
  json_str <- gsub("  +", " ", json_str)
  
  json_str
}

#' @keywords internal
#' Helper: Check if string starts with object brace
#'
#' @noRd
starts_with_brace <- function(s) {
  substr(s, 1, 1) == "{"
}

#' @keywords internal
#' Helper: Convert R types for JSON serialization
#'
#' Pre-processes R objects before jsonlite serialization to handle special
#' types like factors, dates, and data frames.
#'
#' @param x An R object.
#'
#' @return The object, potentially modified for safe JSON serialization.
#'
#' @noRd
convert_r_type_for_json <- function(x) {
  # Keep special types as-is
  if (is_js_expression(x)) {
    return(x)
  }

  # Empty vectors and NULL -> empty array
  # In R, c() returns NULL, so this converts both to empty arrays
  if (is.null(x)) {
    return(list())  # Converts to [] in JSON
  }

  # Factor -> character
  if (is.factor(x)) {
    return(as.character(x))
  }

  # POSIXct/POSIXlt/Date -> ISO 8601 character
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%OSZ"))
  }

  if (inherits(x, "Date")) {
    return(as.character(x))
  }

  # data.frame -> list of lists (rows as objects)
  if (is.data.frame(x)) {
    # Convert each column appropriately
    cols <- lapply(x, convert_r_type_for_json)
    # Transpose to rows
    n_rows <- nrow(x)
    rows <- lapply(
      seq_len(n_rows),
      function(i) lapply(cols, function(col) col[[i]])
    )
    return(rows)
  }

  # List -> recursively convert elements
  if (is.list(x) && !inherits(x, "htmltools.tag")) {
    return(lapply(x, convert_r_type_for_json))
  }

  # Return as-is for atomic types (logical, numeric, character)
  x
}
