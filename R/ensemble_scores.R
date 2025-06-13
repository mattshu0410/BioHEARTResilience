#' Create ensemble risk score from multiple cardiovascular risk scores
#'
#' Combines multiple risk scores using ordered quantile normalization and standardization,
#' following the methodology from the BioHEART resilience paper. For subjects with missing
#' scores, averages available scores with informative warnings.
#'
#' @param risk_scores Data frame with calculated risk scores (from calculate_risk_scores)
#' @param score_cols Character vector of score column names to include in ensemble.
#'   If NULL, automatically detects columns ending in "_10y"
#' @param min_scores Minimum number of valid scores required per row (default: 1)
#' @return Data frame with normalized scores and ensemble average (preserves row names)
#' @export
#' @examples
#' \dontrun{
#' # Create ensemble from all available scores
#' ensemble <- ensemble_risk_score(risk_scores)
#'
#' # Create ensemble from specific scores only
#' ensemble <- ensemble_risk_score(risk_scores,
#'                                score_cols = c("frs_10y", "ascvd_10y"))
#' }
ensemble_risk_score <- function(risk_scores,
                               score_cols = NULL,
                               min_scores = 1) {

  # Validate input
  if (!inherits(risk_scores, "data.frame")) {
    stop("risk_scores must be a data frame")
  }

  # Auto-detect score columns if not specified
  if (is.null(score_cols)) {
    score_cols <- grep("_10y$", names(risk_scores), value = TRUE)
  }

  # Validate score columns exist
  missing_cols <- setdiff(score_cols, names(risk_scores))
  if (length(missing_cols) > 0) {
    stop(sprintf("Score columns not found: %s", paste(missing_cols, collapse = ", ")))
  }

  if (length(score_cols) == 0) {
    stop("No valid score columns found")
  }

  # Initialize results preserving row names and ID column if present
  results <- data.frame(row.names = rownames(risk_scores), stringsAsFactors = FALSE)

  # Preserve ID column if it exists in the input risk_scores data
  if ("id" %in% names(risk_scores)) {
    results$id <- risk_scores$id
  }

  # Track missing patterns for detailed warnings
  missing_patterns <- data.frame(row.names = rownames(risk_scores))

  # Apply ordered quantile normalization and standardization to each score
  normalized_cols <- character(0)


  for (score_col in score_cols) {
    score_values <- risk_scores[[score_col]]

    # Track missing values for this score
    missing_patterns[[score_col]] <- is.na(score_values)

    # Skip if all values are missing
    if (all(is.na(score_values))) {
      warning(sprintf("All values missing for %s, skipping from ensemble", score_col))
      next
    }

    # Apply ordered quantile normalization
    tryCatch({

      ordernorm_result <- bestNormalize::orderNorm(score_values)
      normalized_values <- ordernorm_result$x.t

      # Standardize to mean=0, sd=1
      standardized_values <- standardize(normalized_values, na.rm = TRUE)

      # Store result with new column name
      new_col_name <- paste0("ordernorm_", gsub("_10y$", "", score_col))
      results[[new_col_name]] <- standardized_values
      normalized_cols <- c(normalized_cols, new_col_name)

    }, error = function(e) {
      warning(sprintf("Error normalizing %s: %s. Excluding from ensemble.", score_col, e$message))
      missing_patterns[[score_col]] <- TRUE  # Mark all as missing if normalization failed
    })
  }

  # Calculate ensemble average score
  if (length(normalized_cols) > 0) {
    # Count valid scores per row
    valid_counts <- rowSums(!is.na(results[normalized_cols]))

    # Calculate average for rows with minimum required scores
    results$average_norm_score <- ifelse(
      valid_counts >= min_scores,
      rowMeans(results[normalized_cols], na.rm = TRUE),
      NA
    )

    # Add count of scores used
    results$n_scores_used <- valid_counts

    # Provide detailed warnings about missing scores
    provide_missing_warnings(missing_patterns, score_cols, min_scores, valid_counts)

  } else {
    warning("No scores successfully normalized. Cannot create ensemble score.")
    results$average_norm_score <- NA
    results$n_scores_used <- 0
  }

  # Add attributes for tracking
  attr(results, "score_columns_used") <- score_cols
  attr(results, "normalized_columns") <- normalized_cols
  attr(results, "normalization_method") <- "orderNorm + standardization"
  attr(results, "min_scores_required") <- min_scores
  attr(results, "ensemble_date") <- Sys.Date()

  return(results)
}

#' Provide detailed warnings about missing risk scores
#'
#' @param missing_patterns Data frame indicating missing values for each score
#' @param score_cols Original score columns attempted
#' @param min_scores Minimum scores required
#' @param valid_counts Number of valid scores per row
#' @keywords internal
provide_missing_warnings <- function(missing_patterns, score_cols, min_scores, valid_counts) {

  # Overall summary
  total_subjects <- nrow(missing_patterns)
  insufficient <- sum(valid_counts < min_scores)

  if (insufficient > 0) {
    warning(sprintf(
      "%d of %d subjects (%.1f%%) have fewer than %d valid scores and will have NA ensemble score",
      insufficient, total_subjects, 100 * insufficient / total_subjects, min_scores
    ))
  }

  # Score-specific missing patterns
  for (score_col in score_cols) {
    if (score_col %in% names(missing_patterns)) {
      n_missing <- sum(missing_patterns[[score_col]], na.rm = TRUE)
      if (n_missing > 0) {
        pct_missing <- 100 * n_missing / total_subjects
        score_name <- get_score_friendly_name(score_col)

        # Provide likely reasons for missing scores
        reasons <- get_missing_reasons(score_col)

        message(sprintf(
          "%s missing for %d subjects (%.1f%%). Likely reasons: %s",
          score_name, n_missing, pct_missing, reasons
        ))
      }
    }
  }

  # Pattern analysis for subjects with partial scores
  partial_subjects <- sum(valid_counts > 0 & valid_counts < length(score_cols))
  if (partial_subjects > 0) {
    message(sprintf(
      "%d subjects have partial risk score coverage and will use ensemble of available scores",
      partial_subjects
    ))
  }
}

#' Get friendly name for risk score
#'
#' @param score_col Score column name
#' @return Friendly name for display
#' @keywords internal
get_score_friendly_name <- function(score_col) {
  friendly_names <- c(
    "frs_10y" = "Framingham Risk Score (FRS)",
    "ascvd_10y" = "ASCVD Risk Score",
    "mesa_10y" = "MESA CHD Risk Score",
    "score2_10y" = "SCORE2 Risk Score"
  )

  return(friendly_names[score_col] %||% score_col)
}

#' Get likely reasons for missing risk scores
#'
#' @param score_col Score column name
#' @return String describing likely reasons for missing values
#' @keywords internal
get_missing_reasons <- function(score_col) {
  reasons <- list(
    "frs_10y" = "age outside 30-74 years",
    "ascvd_10y" = "age outside 20-79 years, cholesterol/HDL/BP outside valid ranges, or missing ethnicity",
    "mesa_10y" = "age outside 45-85 years, missing ethnicity, or missing optional variables",
    "score2_10y" = "calculation errors or missing variables"
  )

  return(reasons[[score_col]] %||% "unknown calculation issues")
}

#' Null coalescing operator
#'
#' @param x First value
#' @param y Second value (returned if x is NULL)
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Normalize individual risk scores using ordered quantile normalization
#'
#' Applies ordered quantile normalization to transform risk scores to approximately
#' normal distribution, then standardizes to mean=0, sd=1.
#'
#' @param x Numeric vector of risk scores
#' @param return_transform Logical, whether to return the transformation object
#' @return If return_transform=FALSE (default), returns normalized values.
#'   If return_transform=TRUE, returns list with values and transform object.
#' @export
#' @examples
#' \dontrun{
#' # Normalize a vector of risk scores
#' normalized <- normalize_risk_score(c(5.2, 10.1, 15.6, 20.3))
#'
#' # Get both values and transformation object
#' result <- normalize_risk_score(c(5.2, 10.1, 15.6, 20.3), return_transform = TRUE)
#' }
normalize_risk_score <- function(x, return_transform = FALSE) {

  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }

  if (all(is.na(x))) {
    warning("All values are missing")
    if (return_transform) {
      return(list(values = x, transform = NULL))
    } else {
      return(x)
    }
  }

  tryCatch({
    # Apply ordered quantile normalization
    ordernorm_result <- bestNormalize::orderNorm(x)
    normalized_values <- ordernorm_result$x.t

    # Standardize to mean=0, sd=1
    standardized_values <- standardize(normalized_values, na.rm = TRUE)

    if (return_transform) {
      return(list(
        values = standardized_values,
        transform = ordernorm_result,
        original_stats = list(
          mean = mean(x, na.rm = TRUE),
          sd = stats::sd(x, na.rm = TRUE),
          min = min(x, na.rm = TRUE),
          max = max(x, na.rm = TRUE)
        )
      ))
    } else {
      return(standardized_values)
    }

  }, error = function(e) {
    warning(sprintf("Error in normalization: %s", e$message))
    if (return_transform) {
      return(list(values = rep(NA, length(x)), transform = NULL))
    } else {
      return(rep(NA, length(x)))
    }
  })
}

#' Get ensemble score summary
#'
#' Creates a summary of the ensemble scoring process including score distributions
#' and missing data patterns.
#'
#' @param ensemble_scores Data frame from ensemble_risk_score()
#' @param original_scores Optional data frame with original risk scores for comparison
#' @return List containing summary statistics and diagnostics
#' @export
summarize_ensemble_scores <- function(ensemble_scores, original_scores = NULL) {

  # Get normalized score columns
  norm_cols <- grep("^ordernorm_", names(ensemble_scores), value = TRUE)

  # Summary statistics for normalized scores
  norm_summary <- lapply(ensemble_scores[norm_cols], function(x) {
    c(
      n = sum(!is.na(x)),
      n_missing = sum(is.na(x)),
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  })

  norm_summary_df <- do.call(rbind, norm_summary)

  # Ensemble score summary
  ensemble_summary <- c(
    n = sum(!is.na(ensemble_scores$average_norm_score)),
    n_missing = sum(is.na(ensemble_scores$average_norm_score)),
    mean = mean(ensemble_scores$average_norm_score, na.rm = TRUE),
    sd = stats::sd(ensemble_scores$average_norm_score, na.rm = TRUE),
    min = min(ensemble_scores$average_norm_score, na.rm = TRUE),
    max = max(ensemble_scores$average_norm_score, na.rm = TRUE)
  )

  # Score usage patterns
  if ("n_scores_used" %in% names(ensemble_scores)) {
    usage_table <- table(ensemble_scores$n_scores_used)
  } else {
    usage_table <- NULL
  }

  # Correlation matrix of normalized scores
  if (length(norm_cols) > 1) {
    cor_matrix <- stats::cor(ensemble_scores[norm_cols], use = "pairwise.complete.obs")
  } else {
    cor_matrix <- NULL
  }

  result <- list(
    normalized_summary = norm_summary_df,
    ensemble_summary = ensemble_summary,
    score_usage = usage_table,
    correlations_normalized = cor_matrix,
    n_complete_ensemble = sum(!is.na(ensemble_scores$average_norm_score))
  )

  # Add comparison with original scores if provided
  if (!is.null(original_scores)) {
    orig_cols <- grep("_10y$", names(original_scores), value = TRUE)
    if (length(orig_cols) > 1) {
      result$correlations_original <- stats::cor(original_scores[orig_cols],
                                               use = "pairwise.complete.obs")
    }
  }

  class(result) <- c("ensemble_summary", "list")
  return(result)
}

#' Print method for ensemble summary
#'
#' @param x ensemble_summary object
#' @param ... Additional arguments (ignored)
#' @export
print.ensemble_summary <- function(x, ...) {
  cat("Ensemble Risk Score Summary\n")
  cat("===========================\n\n")

  cat("Normalized Score Statistics:\n")
  print(round(x$normalized_summary, 3))

  cat("\n\nEnsemble Score Statistics:\n")
  print(round(x$ensemble_summary, 3))

  if (!is.null(x$score_usage)) {
    cat("\n\nNumber of Scores Used Per Subject:\n")
    print(x$score_usage)
  }

  if (!is.null(x$correlations_normalized)) {
    cat("\n\nCorrelations (Normalized Scores):\n")
    print(round(x$correlations_normalized, 3))
  }

  cat(sprintf("\n\nComplete ensemble scores: %d\n", x$n_complete_ensemble))

  invisible(x)
}
