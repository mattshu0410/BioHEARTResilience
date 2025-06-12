#' Calculate cardiovascular risk scores
#'
#' Calculates multiple cardiovascular risk scores using CVrisk and RiskScorescvd packages
#'
#' @param data Prepared data frame from prepare_cohort_data()
#' @param scores Character vector of scores to calculate. Options: "frs", "ascvd", "mesa", "score2"
#' @param ethnicity_mappings Optional custom ethnicity mappings
#' @param risk_region Risk region for SCORE2 ("Low", "Moderate", "High", or "Very high")
#' @param handle_missing How to handle missing scores ("exclude" or "na")
#' @return Data frame with calculated risk scores (preserves row names)
#' @export
#' @examples
#' \dontrun{
#' # Calculate all available risk scores
#' risk_scores <- calculate_risk_scores(
#'   prepared_data,
#'   scores = c("frs", "ascvd", "mesa", "score2")
#' )
#' }
calculate_risk_scores <- function(data,
                                scores = c("frs", "ascvd", "mesa", "score2"),
                                ethnicity_mappings = NULL,
                                risk_region = "Low",
                                handle_missing = c("exclude", "na")) {
  
  handle_missing <- match.arg(handle_missing)
  
  # Validate input
  if (!inherits(data, "data.frame")) {
    stop("data must be a data frame")
  }
  
  # Initialize results preserving row names
  results <- data.frame(row.names = rownames(data), stringsAsFactors = FALSE)
  
  # Add ethnicity mappings if not already present
  if (any(c("ascvd", "mesa") %in% scores) && !"ethnicity_ascvd" %in% names(data)) {
    if ("ethnicity" %in% names(data)) {
      eth_mapped <- recode_ethnicity(data$ethnicity, target_score = "all", 
                                    mapping_table = ethnicity_mappings)
      data$ethnicity_ascvd <- eth_mapped$ethnicity_ascvd
      data$ethnicity_mesa <- eth_mapped$ethnicity_mesa
    } else {
      warning("No ethnicity column found. Using default ethnicity for risk scores.")
      data$ethnicity_ascvd <- "white"
      data$ethnicity_mesa <- "white"
    }
  }
  
  # Calculate FRS if requested
  if ("frs" %in% scores) {
    tryCatch({
      results$frs_10y <- CVrisk::ascvd_10y_frs(
        gender = data$gender,
        age = data$age,
        hdl = data$hdl_mgdl,
        totchol = data$tc_mgdl,
        sbp = data$sbp,
        bp_med = data$bp_med,
        smoker = data$curr_smok,
        diabetes = data$cvhx_dm
      )
    }, error = function(e) {
      warning(sprintf("Error calculating FRS: %s", e$message))
      results$frs_10y <- NA
    })
  }
  
  # Calculate ASCVD if requested
  if ("ascvd" %in% scores) {
    tryCatch({
      results$ascvd_10y <- CVrisk::ascvd_10y_accaha(
        race = data$ethnicity_ascvd,
        gender = data$gender,
        age = data$age,
        totchol = data$tc_mgdl,
        hdl = data$hdl_mgdl,
        sbp = data$sbp,
        bp_med = data$bp_med,
        smoker = data$curr_smok,
        diabetes = data$cvhx_dm
      )
    }, error = function(e) {
      warning(sprintf("Error calculating ASCVD: %s", e$message))
      results$ascvd_10y <- NA
    })
  }
  
  # Calculate MESA if requested
  if ("mesa" %in% scores) {
    # Set default values for optional parameters
    if (!"lipid_med" %in% names(data)) data$lipid_med <- 0
    if (!"fh_ihd" %in% names(data)) data$fh_ihd <- 0
    
    tryCatch({
      results$mesa_10y <- CVrisk::chd_10y_mesa(
        race = data$ethnicity_mesa,
        gender = data$gender,
        age = data$age,
        totchol = data$tc_mgdl,
        hdl = data$hdl_mgdl,
        lipid_med = data$lipid_med,
        sbp = data$sbp,
        bp_med = data$bp_med,
        smoker = data$curr_smok,
        diabetes = data$cvhx_dm,
        fh_heartattack = data$fh_ihd
      )
    }, error = function(e) {
      warning(sprintf("Error calculating MESA: %s", e$message))
      results$mesa_10y <- NA
    })
  }
  
  # Calculate SCORE2 if requested
  if ("score2" %in% scores) {
    tryCatch({
      results$score2_10y <- RiskScorescvd::SCORE2(
        Risk.region = risk_region,
        Age = data$age,
        Gender = data$gender,
        smoker = data$curr_smok,
        systolic.bp = data$sbp,
        diabetes = data$cvhx_dm,
        total.chol = data$tc,      # SCORE2 expects mmol/L
        total.hdl = data$hdl,       # SCORE2 expects mmol/L
        classify = FALSE
      )
    }, error = function(e) {
      warning(sprintf("Error calculating SCORE2: %s", e$message))
      results$score2_10y <- NA
    })
  }
  
  # Handle missing values based on preference
  if (handle_missing == "exclude") {
    # Count how many scores were successfully calculated for each row
    score_cols <- grep("_10y$", names(results), value = TRUE)
    if (length(score_cols) > 0) {
      results$n_scores <- rowSums(!is.na(results[score_cols]))
      
      # Warn about rows with no valid scores
      no_scores <- sum(results$n_scores == 0)
      if (no_scores > 0) {
        warning(sprintf("%d rows have no valid risk scores calculated", no_scores))
      }
    }
  }
  
  # Add attributes
  attr(results, "scores_calculated") <- scores
  attr(results, "risk_region") <- risk_region
  attr(results, "calculation_date") <- Sys.Date()
  
  return(results)
}

#' Get risk score information
#'
#' Returns information about available risk scores and their requirements
#'
#' @param score Optional specific score to get information for
#' @return Data frame with risk score information
#' @export
#' @examples
#' # Get information for all scores
#' risk_score_info()
#' 
#' # Get information for specific score
#' risk_score_info("ascvd")
risk_score_info <- function(score = NULL) {
  
  info <- data.frame(
    score = c("frs", "ascvd", "mesa", "score2"),
    full_name = c(
      "Framingham Risk Score",
      "ACC/AHA ASCVD Pooled Cohort Equations",
      "MESA CHD Risk Score",
      "SCORE2"
    ),
    outcome = c(
      "10-year ASCVD risk",
      "10-year hard ASCVD risk",
      "10-year CHD risk",
      "10-year fatal and non-fatal CVD risk"
    ),
    age_range = c(
      "30-74",
      "20-79",
      "45-85",
      "40-75 (optimal)"
    ),
    ethnicity_required = c(
      FALSE,
      TRUE,
      TRUE,
      FALSE
    ),
    special_requirements = c(
      "None",
      "TC: 130-320 mg/dL, HDL: 20-100 mg/dL, SBP: 90-200 mmHg",
      "Optional: family history, lipid medication",
      "Cholesterol in mmol/L, risk region specification"
    ),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(score)) {
    info <- info[info$score == score, ]
    if (nrow(info) == 0) {
      stop(sprintf("Unknown score: %s. Available scores: %s", 
                  score, paste(info$score, collapse = ", ")))
    }
  }
  
  return(info)
}

#' Summarize Risk Score Calculation Results
#'
#' Creates a summary of risk score calculation results including coverage
#' statistics, missing data patterns, and data quality metrics.
#'
#' @param risk_scores Data frame with calculated risk scores
#' @param original_data Original prepared data (optional)
#' @return List with summary statistics including n_complete, coverage, etc.
#' @export
#' @examples
#' \dontrun{
#' # Calculate risk scores
#' risk_results <- calculate_risk_scores(data)
#' 
#' # Summarize results
#' summary <- summarize_risk_scores(risk_results, data)
#' print(summary$n_complete)
#' }
summarize_risk_scores <- function(risk_scores, original_data = NULL) {
  
  if (!inherits(risk_scores, "data.frame")) {
    stop("risk_scores must be a data frame")
  }
  
  # Identify risk score columns
  score_cols <- grep("_10y$", names(risk_scores), value = TRUE)
  
  if (length(score_cols) == 0) {
    warning("No risk score columns found (expected columns ending in '_10y')")
    return(list(
      n_complete = 0,
      n_total = nrow(risk_scores),
      coverage = 0,
      score_columns = character(0)
    ))
  }
  
  # Calculate completeness for each score
  score_completeness <- sapply(score_cols, function(col) {
    sum(!is.na(risk_scores[[col]]))
  })
  
  # Calculate overall completeness (subjects with at least one score)
  score_matrix <- risk_scores[, score_cols, drop = FALSE]
  has_any_score <- rowSums(!is.na(score_matrix)) > 0
  n_complete <- sum(has_any_score)
  n_total <- nrow(risk_scores)
  
  # Calculate subjects with all scores
  has_all_scores <- rowSums(!is.na(score_matrix)) == length(score_cols)
  n_all_complete <- sum(has_all_scores)
  
  # Create summary object
  summary_obj <- list(
    # Basic counts
    n_complete = n_complete,
    n_all_complete = n_all_complete,
    n_total = n_total,
    
    # Coverage statistics
    coverage = n_complete / n_total,
    all_scores_coverage = n_all_complete / n_total,
    
    # Score-specific statistics
    score_columns = score_cols,
    score_completeness = score_completeness,
    score_coverage = score_completeness / n_total,
    
    # Missing patterns
    missing_any = n_total - n_complete,
    missing_all = sum(rowSums(!is.na(score_matrix)) == 0)
  )
  
  class(summary_obj) <- "risk_score_summary"
  return(summary_obj)
}

#' Print Risk Score Summary
#'
#' @param x A risk_score_summary object
#' @param ... Additional arguments (ignored)
#' @export
print.risk_score_summary <- function(x, ...) {
  cat("Risk Score Summary\n")
  cat("==================\n\n")
  
  cat(sprintf("Total subjects: %d\n", x$n_total))
  cat(sprintf("Subjects with any risk score: %d (%.1f%%)\n", 
              x$n_complete, 100 * x$coverage))
  cat(sprintf("Subjects with all risk scores: %d (%.1f%%)\n", 
              x$n_all_complete, 100 * x$all_scores_coverage))
  
  if (x$missing_any > 0) {
    cat(sprintf("Subjects missing any scores: %d (%.1f%%)\n", 
                x$missing_any, 100 * x$missing_any / x$n_total))
  }
  
  if (x$missing_all > 0) {
    cat(sprintf("Subjects missing all scores: %d (%.1f%%)\n", 
                x$missing_all, 100 * x$missing_all / x$n_total))
  }
  
  cat("\nScore-specific coverage:\n")
  for (i in seq_along(x$score_columns)) {
    score_name <- x$score_columns[i]
    n_complete <- x$score_completeness[i]
    coverage <- x$score_coverage[i]
    cat(sprintf("  %s: %d/%d (%.1f%%)\n", 
                score_name, n_complete, x$n_total, 100 * coverage))
  }
  
  invisible(x)
}