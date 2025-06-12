#' Convert cholesterol units from mmol/L to mg/dL
#'
#' @param value Numeric vector of cholesterol values in mmol/L
#' @return Numeric vector of cholesterol values in mg/dL
#' @export
#' @examples
#' convert_cholesterol_units(5.2)  # Returns 201.08
#' convert_cholesterol_units(1.3)  # Returns 50.27
convert_cholesterol_units <- function(value) {
  conversion_factor <- 38.67
  return(value * conversion_factor)
}

#' Check if a value is within a specified range
#'
#' @param x Numeric vector to check
#' @param min Minimum value (inclusive)
#' @param max Maximum value (inclusive)
#' @param var_name Name of the variable for error messages
#' @return Logical vector indicating which values are within range
#' @keywords internal
check_range <- function(x, min, max, var_name) {
  in_range <- x >= min & x <= max
  out_of_range <- sum(!in_range, na.rm = TRUE)
  
  if (out_of_range > 0) {
    warning(sprintf(
      "%d %s values outside range [%g, %g]. These will produce NA results.",
      out_of_range, var_name, min, max
    ))
  }
  return(in_range)
}

#' Validate required columns exist in data
#'
#' @param data Data frame to check
#' @param required_cols Character vector of required column names
#' @return Invisible TRUE if all columns exist, otherwise stops with error
#' @keywords internal
validate_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Required columns missing from data: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }
  invisible(TRUE)
}

#' Check for missing values and provide summary
#'
#' @param data Data frame to check
#' @param cols Columns to check for missing values
#' @param verbose Logical, whether to print missing value summary
#' @return Invisible data frame with missing value counts and percentages
#' @keywords internal
check_missing_values <- function(data, cols = names(data), verbose = TRUE) {
  cols <- intersect(cols, names(data))
  
  missing_counts <- sapply(data[cols], function(x) sum(is.na(x)))
  missing_pct <- 100 * missing_counts / nrow(data)
  
  missing_summary <- data.frame(
    column = names(missing_counts),
    n_missing = unname(missing_counts),
    pct_missing = unname(missing_pct),
    stringsAsFactors = FALSE
  )
  
  missing_summary <- missing_summary[missing_summary$n_missing > 0, ]
  
  if (verbose && nrow(missing_summary) > 0) {
    message("Missing values found:")
    for (i in seq_len(nrow(missing_summary))) {
      message(sprintf("  %s: %d missing (%.1f%%)", 
                      missing_summary$column[i], 
                      missing_summary$n_missing[i],
                      missing_summary$pct_missing[i]))
    }
  }
  
  return(invisible(missing_summary))
}

#' Standardize variable to mean 0 and SD 1
#'
#' @param x Numeric vector to standardize
#' @param na.rm Logical, whether to remove NA values
#' @return Standardized numeric vector
#' @keywords internal
standardize <- function(x, na.rm = TRUE) {
  if (na.rm) {
    mean_x <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)
  } else {
    mean_x <- mean(x)
    sd_x <- sd(x)
  }
  
  if (sd_x == 0) {
    warning("Standard deviation is 0, returning centered values")
    return(x - mean_x)
  }
  
  return((x - mean_x) / sd_x)
}

#' Validate binary variable
#'
#' @param x Vector to check
#' @param var_name Name of variable for error messages
#' @return Invisible TRUE if valid, otherwise stops with error
#' @keywords internal
validate_binary <- function(x, var_name) {
  unique_vals <- unique(x[!is.na(x)])
  
  if (!all(unique_vals %in% c(0, 1))) {
    stop(sprintf(
      "%s must be binary (0/1). Found values: %s",
      var_name,
      paste(unique_vals, collapse = ", ")
    ))
  }
  
  invisible(TRUE)
}

#' Validate gender variable
#'
#' @param x Vector to check
#' @return Character vector with standardized gender values
#' @keywords internal
validate_gender <- function(x) {
  x <- tolower(as.character(x))
  
  # Map common variations
  x[x %in% c("m", "male", "1")] <- "male"
  x[x %in% c("f", "female", "2")] <- "female"
  
  valid_values <- c("male", "female", NA)
  invalid <- !x %in% valid_values
  
  if (any(invalid)) {
    stop(sprintf(
      "Invalid gender values found: %s. Gender must be 'male' or 'female'.",
      paste(unique(x[invalid]), collapse = ", ")
    ))
  }
  
  return(x)
}