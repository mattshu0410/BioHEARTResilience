#' Complete Resilience Analysis Pipeline
#'
#' Performs a complete resilience analysis including risk score calculation,
#' ensemble scoring, zero-inflated modeling, percentile calculation, and
#' resilience classification.
#'
#' @param data Data frame with prepared clinical and CACS data (from prepare_cohort_data, missing CACS already removed)
#' @param risk_scores Character vector of risk scores to calculate (default: all available)
#' @param risk_region Risk region for SCORE2 (default: "Low")
#' @param percentile_thresholds Named vector of classification thresholds
#' @param min_scores Minimum number of risk scores required for ensemble (default: 1)
#' @param include_plots Logical, whether to generate diagnostic plots (default: TRUE)
#' @param ethnicity_mappings Optional data frame with custom ethnicity mappings for risk scores
#' @return List containing all analysis results and optional plots
#' @export
#' @examples
#' \dontrun{
#' # Step 1: Prepare cohort data
#' prepared_data <- prepare_cohort_data(
#'   raw_data,
#'   cholesterol_unit = "mmol/L",
#'   validate = TRUE
#' )
#'
#' # Step 2: Run resilience analysis
#' results <- resilience_analysis(prepared_data)
#'
#' # Access results
#' results$classifications$classification
#' results$model_summary
#' results$plots$cacs_vs_risk
#'
#' # Custom analysis with ethnicity mappings
#' custom_mappings <- data.frame(
#'   original = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
#'   ascvd = c("white", "other", "other", "aa", "other", "other", "other", "other", "other"),
#'   mesa = c("white", "aa", "white", "aa", "chinese", "white", "white", "hispanic", "white"),
#'   stringsAsFactors = FALSE
#' )
#'
#' prepared_data <- prepare_cohort_data(
#'   raw_data,
#'   ethnicity_col = "ethcat",
#'   id_col = "BioHEART_ID",
#'   cholesterol_unit = "mmol/L"
#' )
#'
#' results <- resilience_analysis(
#'   prepared_data,
#'   risk_scores = c("frs", "ascvd", "mesa"),
#'   ethnicity_mappings = custom_mappings,
#'   percentile_thresholds = c(resilient = 15, reference_low = 35,
#'                           reference_high = 65, susceptible = 85),
#'   include_plots = FALSE
#' )
#' }
resilience_analysis <- function(data,
                               risk_scores = c("frs", "ascvd", "mesa", "score2"),
                               risk_region = "Low",
                               percentile_thresholds = c(resilient = 20, reference_low = 40,
                                                       reference_high = 60, susceptible = 80),
                               min_scores = 1,
                               include_plots = TRUE,
                               ethnicity_mappings = NULL) {

  message("Starting BioHEART Resilience Analysis...")
  message("=====================================")

  # Step 1: Risk Score Calculation
  message("\n1. Calculating cardiovascular risk scores...")

  tryCatch({
    risk_score_results <- calculate_risk_scores(
      data,
      scores = risk_scores,
      ethnicity_mappings = ethnicity_mappings,
      risk_region = risk_region
    )

    # Summary of risk scores
    risk_summary <- summarize_risk_scores(risk_score_results, data)
    message(sprintf("   ✓ Calculated %d risk score types", length(risk_scores)))
    message(sprintf("   ✓ Complete risk data for %d subjects", risk_summary$n_complete))

  }, error = function(e) {
    stop(sprintf("Error in risk score calculation: %s", e$message))
  })

  # Step 2: Ensemble Scoring
  message("\n2. Creating ensemble risk scores...")

  tryCatch({
    ensemble_results <- ensemble_risk_score(
      risk_score_results,
      min_scores = min_scores
    )

    n_ensemble <- sum(!is.na(ensemble_results$average_norm_score))
    message(sprintf("   ✓ Created ensemble scores for %d subjects", n_ensemble))

  }, error = function(e) {
    stop(sprintf("Error in ensemble scoring: %s", e$message))
  })

  # Step 3: Combine data for modeling
  message("\n3. Preparing data for modeling...")

  # Merge only rows that exist in both data and ensemble_results (inner join)
  modeling_data <- merge(data, ensemble_results, by = "row.names", all = FALSE)
  rownames(modeling_data) <- modeling_data$Row.names
  modeling_data$Row.names <- NULL

  # Handle ID column duplication from merge (id.x from data, id.y from ensemble_results)
  if ("id.x" %in% names(modeling_data) && "id.y" %in% names(modeling_data)) {
    # They should be identical, so keep one and remove the other
    modeling_data$id <- modeling_data$id.x
    modeling_data$id.x <- NULL
    modeling_data$id.y <- NULL
  } else if ("id.x" %in% names(modeling_data)) {
    modeling_data$id <- modeling_data$id.x
    modeling_data$id.x <- NULL
  } else if ("id.y" %in% names(modeling_data)) {
    modeling_data$id <- modeling_data$id.y
    modeling_data$id.y <- NULL
  }

  # Reorder columns to put ID first if it exists
  if ("id" %in% names(modeling_data)) {
    modeling_data <- modeling_data[, c("id", setdiff(names(modeling_data), "id"))]
  }

  # Remove rows without both CACS and ensemble score
  complete_for_modeling <- stats::complete.cases(modeling_data$cacs,
                                         modeling_data$average_norm_score)
  n_modeling <- sum(complete_for_modeling)

  if (n_modeling == 0) {
    stop("No subjects have both CACS and ensemble risk scores for modeling")
  }

  message(sprintf("   ✓ %d subjects available for resilience modeling", n_modeling))

  # Step 4: Zero-Inflated Model Fitting
  message("\n4. Fitting zero-inflated CACS model...")

  tryCatch({
    cacs_model <- fit_cacs_model(
      modeling_data,
      cacs_col = "cacs",
      risk_score_col = "average_norm_score"
    )
    message("   ✓ Zero-inflated model fitted successfully")

  }, error = function(e) {
    stop(sprintf("Error fitting CACS model: %s", e$message))
  })

  # Step 5: Percentile Calculation
  message("\n5. Calculating risk-adjusted CACS percentiles...")

  tryCatch({
    percentile_results <- calculate_cacs_percentiles(
      cacs_model,
      modeling_data,
      cacs_col = "cacs",
      risk_score_col = "average_norm_score"
    )

    n_percentiles <- sum(!is.na(percentile_results$cacs_percentile))
    message(sprintf("   ✓ Calculated percentiles for %d subjects", n_percentiles))
    
    # Reorder percentile_results to put ID first if it exists
    if ("id" %in% names(percentile_results)) {
      percentile_results <- percentile_results[, c("id", setdiff(names(percentile_results), "id"))]
    }

  }, error = function(e) {
    stop(sprintf("Error calculating percentiles: %s", e$message))
  })

  # Step 6: Resilience Classification
  message("\n6. Classifying resilience phenotypes...")

  tryCatch({
    classification_results <- classify_resilience(
      percentile_results,
      thresholds = percentile_thresholds
    )

    # Reorder classification_results to put ID first if it exists
    if ("id" %in% names(classification_results)) {
      classification_results <- classification_results[, c("id", setdiff(names(classification_results), "id"))]
    }

  }, error = function(e) {
    stop(sprintf("Error in resilience classification: %s", e$message))
  })

  # Step 7: Model Diagnostics
  message("\n7. Computing model diagnostics...")

  model_diagnostics_results <- model_diagnostics(cacs_model, percentile_results)

  # Step 8: Combine all results
  message("\n8. Combining results...")

  final_data <- merge(modeling_data, classification_results, by = "row.names", all = TRUE)
  rownames(final_data) <- final_data$Row.names
  final_data$Row.names <- NULL
  
  # Handle ID column duplication from merge (id.x from modeling_data, id.y from classification_results)
  if ("id.x" %in% names(final_data) && "id.y" %in% names(final_data)) {
    # They should be identical, so keep one and remove the other
    final_data$id <- final_data$id.x
    final_data$id.x <- NULL
    final_data$id.y <- NULL
  } else if ("id.x" %in% names(final_data)) {
    final_data$id <- final_data$id.x
    final_data$id.x <- NULL
  } else if ("id.y" %in% names(final_data)) {
    final_data$id <- final_data$id.y
    final_data$id.y <- NULL
  }

  # Reorder columns to put ID first if it exists
  if ("id" %in% names(final_data)) {
    final_data <- final_data[, c("id", setdiff(names(final_data), "id"))]
  }

  # Create results object
  results <- list(
    # Data
    prepared_data = data,
    risk_scores = risk_score_results,
    ensemble_scores = ensemble_results,
    percentiles = percentile_results,
    classifications = classification_results,
    final_data = final_data,

    # Model
    model = cacs_model,
    model_diagnostics = model_diagnostics_results,

    # Summaries
    risk_summary = risk_summary,
    ensemble_summary = summarize_ensemble_scores(ensemble_results, risk_score_results),

    # Parameters used
    parameters = list(
      risk_scores = risk_scores,
      risk_region = risk_region,
      percentile_thresholds = percentile_thresholds,
      min_scores = min_scores
    )
  )

  # Step 9: Generate plots if requested
  if (include_plots) {
    message("\n9. Generating diagnostic plots...")

    tryCatch({
      results$plots <- list()

      # Main resilience plot
      results$plots$cacs_vs_risk <- plot_cacs_vs_risk(
        final_data,
        cacs_col = "cacs",
        risk_score_col = "average_norm_score",
        class_col = "classification"
      )

      # Risk distribution plot
      results$plots$risk_distribution <- plot_risk_distribution(
        final_data,
        risk_score_col = "average_norm_score",
        class_col = "classification"
      )

      # Percentile distribution
      results$plots$percentile_distribution <- plot_percentile_distribution(
        final_data,
        percentile_col = "cacs_percentile"
      )

      # Model diagnostics plots
      results$plots$model_diagnostics <- plot_model_diagnostics(
        cacs_model,
        percentile_results
      )

      message("   ✓ Generated diagnostic plots")

    }, error = function(e) {
      warning(sprintf("Error generating plots: %s", e$message))
      results$plots <- NULL
    })
  }

  # Final summary
  class_summary <- attr(classification_results, "class_summary")
  message("\n" %+% paste(rep("=", 50), collapse = ""))
  message("RESILIENCE ANALYSIS COMPLETE")
  message(paste(rep("=", 50), collapse = ""))
  message(sprintf("Total subjects analyzed: %d", nrow(final_data)))
  message(sprintf("Subjects with complete data: %d", n_modeling))
  message("\nResilience Classification Results:")
  message(sprintf("  • Resilient: %d subjects", class_summary["resilient"]))
  message(sprintf("  • Reference: %d subjects", class_summary["reference"]))
  message(sprintf("  • Susceptible: %d subjects", class_summary["susceptible"]))
  message(sprintf("  • Other: %d subjects", class_summary["other"]))
  message(sprintf("  • Missing: %d subjects", class_summary["missing"]))

  # Add class and attributes
  class(results) <- c("resilience_analysis", "list")
  attr(results, "analysis_date") <- Sys.Date()
  attr(results, "package_version") <- utils::packageVersion("BioHEARTResilience")

  return(results)
}

#' Print method for resilience analysis results
#'
#' @param x resilience_analysis object
#' @param ... Additional arguments (ignored)
#' @export
print.resilience_analysis <- function(x, ...) {
  cat("BioHEART Resilience Analysis Results\n")
  cat("====================================\n\n")

  # Basic summary
  n_total <- nrow(x$final_data)
  n_complete <- x$model_diagnostics$fit_statistics$n_observations

  cat(sprintf("Subjects analyzed: %d\n", n_total))
  cat(sprintf("Complete data for modeling: %d\n", n_complete))
  cat(sprintf("Analysis date: %s\n\n", attr(x, "analysis_date")))

  # Risk scores used
  cat("Risk scores calculated:\n")
  for (score in x$parameters$risk_scores) {
    cat(sprintf("  • %s\n", score))
  }
  cat("\n")

  # Classification summary
  class_summary <- attr(x$classifications, "class_summary")
  cat("Resilience Classifications:\n")
  for (class_name in names(class_summary)) {
    pct <- 100 * class_summary[class_name] / n_total
    cat(sprintf("  • %s: %d (%.1f%%)\n",
                tools::toTitleCase(class_name),
                class_summary[class_name],
                pct))
  }
  cat("\n")

  # Model fit
  cat("Model Performance:\n")
  cat(sprintf("  • Log-likelihood: %.2f\n", x$model_diagnostics$fit_statistics$log_likelihood))
  cat(sprintf("  • AIC: %.2f\n", x$model_diagnostics$fit_statistics$aic))
  cat(sprintf("  • Predicted-Observed Correlation: %.3f\n", x$model_diagnostics$predicted_observed_correlation))

  if (!is.null(x$model_diagnostics$percentile_uniformity_test)) {
    cat(sprintf("  • Percentile Uniformity p-value: %.4f\n",
                x$model_diagnostics$percentile_uniformity_test$p.value))
  }

  cat("\nAccess results with:\n")
  cat("  • $classifications - Resilience classifications\n")
  cat("  • $final_data - Complete analysis dataset\n")
  cat("  • $model - Zero-inflated CACS model\n")
  cat("  • $plots - Diagnostic plots (if generated)\n")

  invisible(x)
}

#' String concatenation operator
#'
#' @param x First string
#' @param y Second string
#' @return Concatenated string
#' @keywords internal
`%+%` <- function(x, y) {
  paste0(x, y)
}
