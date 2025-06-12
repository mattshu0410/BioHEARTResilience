#' Complete Resilience Analysis Pipeline
#'
#' Performs a complete resilience analysis including risk score calculation,
#' ensemble scoring, zero-inflated modeling, percentile calculation, and 
#' resilience classification.
#'
#' @param data Data frame with clinical and CACS data
#' @param cacs_col Name of CACS column (default: "cacs")
#' @param risk_scores Character vector of risk scores to calculate (default: all available)
#' @param risk_region Risk region for SCORE2 (default: "Low")
#' @param percentile_thresholds Named vector of classification thresholds
#' @param min_scores Minimum number of risk scores required for ensemble (default: 1)
#' @param include_plots Logical, whether to generate diagnostic plots (default: TRUE)
#' @param cholesterol_unit Unit of cholesterol values (default: "mmol/L")
#' @param validate_data Logical, whether to validate input data (default: TRUE)
#' @return List containing all analysis results and optional plots
#' @export
#' @examples
#' \dontrun{
#' # Complete analysis with default settings
#' results <- resilience_analysis(my_data)
#' 
#' # Access results
#' results$classifications$resilience_class
#' results$model_summary
#' results$plots$cacs_vs_risk
#' 
#' # Custom analysis
#' results <- resilience_analysis(
#'   my_data,
#'   risk_scores = c("frs", "ascvd", "mesa"),
#'   percentile_thresholds = c(resilient = 15, reference_low = 35,
#'                           reference_high = 65, susceptible = 85),
#'   include_plots = FALSE
#' )
#' }
resilience_analysis <- function(data,
                               cacs_col = "cacs",
                               risk_scores = c("frs", "ascvd", "mesa", "score2"),
                               risk_region = "Low",
                               percentile_thresholds = c(resilient = 20, reference_low = 40,
                                                       reference_high = 60, susceptible = 80),
                               min_scores = 1,
                               include_plots = TRUE,
                               cholesterol_unit = c("mmol/L", "mg/dL"),
                               validate_data = TRUE) {
  
  cholesterol_unit <- match.arg(cholesterol_unit)
  
  message("Starting BioHEART Resilience Analysis...")
  message("=====================================")
  
  # Step 1: Data Preparation
  message("\n1. Preparing cohort data...")
  
  tryCatch({
    prepared_data <- prepare_cohort_data(
      data,
      cacs_col = cacs_col,
      cholesterol_unit = cholesterol_unit,
      validate = validate_data
    )
    message(sprintf("   ✓ Prepared data for %d subjects", nrow(prepared_data)))
  }, error = function(e) {
    stop(sprintf("Error in data preparation: %s", e$message))
  })
  
  # Step 2: Risk Score Calculation
  message("\n2. Calculating cardiovascular risk scores...")
  
  tryCatch({
    risk_score_results <- calculate_risk_scores(
      prepared_data,
      scores = risk_scores,
      risk_region = risk_region
    )
    
    # Summary of risk scores
    risk_summary <- summarize_risk_scores(risk_score_results, prepared_data)
    message(sprintf("   ✓ Calculated %d risk score types", length(risk_scores)))
    message(sprintf("   ✓ Complete risk data for %d subjects", risk_summary$n_complete))
    
  }, error = function(e) {
    stop(sprintf("Error in risk score calculation: %s", e$message))
  })
  
  # Step 3: Ensemble Scoring
  message("\n3. Creating ensemble risk scores...")
  
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
  
  # Step 4: Combine data for modeling
  message("\n4. Preparing data for modeling...")
  
  # Merge all data together preserving row names
  modeling_data <- merge(prepared_data, ensemble_results, by = "row.names", all = TRUE)
  rownames(modeling_data) <- modeling_data$Row.names
  modeling_data$Row.names <- NULL
  
  # Remove rows without both CACS and ensemble score
  complete_for_modeling <- complete.cases(modeling_data[[cacs_col]], 
                                         modeling_data$average_norm_score)
  n_modeling <- sum(complete_for_modeling)
  
  if (n_modeling == 0) {
    stop("No subjects have both CACS and ensemble risk scores for modeling")
  }
  
  message(sprintf("   ✓ %d subjects available for resilience modeling", n_modeling))
  
  # Step 5: Zero-Inflated Model Fitting
  message("\n5. Fitting zero-inflated CACS model...")
  
  tryCatch({
    cacs_model <- fit_cacs_model(
      modeling_data,
      cacs_col = cacs_col,
      risk_score_col = "average_norm_score"
    )
    message("   ✓ Zero-inflated model fitted successfully")
    
  }, error = function(e) {
    stop(sprintf("Error fitting CACS model: %s", e$message))
  })
  
  # Step 6: Percentile Calculation
  message("\n6. Calculating risk-adjusted CACS percentiles...")
  
  tryCatch({
    percentile_results <- calculate_cacs_percentiles(
      cacs_model,
      modeling_data,
      cacs_col = cacs_col,
      risk_score_col = "average_norm_score"
    )
    
    n_percentiles <- sum(!is.na(percentile_results$cacs_percentile))
    message(sprintf("   ✓ Calculated percentiles for %d subjects", n_percentiles))
    
  }, error = function(e) {
    stop(sprintf("Error calculating percentiles: %s", e$message))
  })
  
  # Step 7: Resilience Classification
  message("\n7. Classifying resilience phenotypes...")
  
  tryCatch({
    classification_results <- classify_resilience(
      percentile_results,
      thresholds = percentile_thresholds
    )
    
  }, error = function(e) {
    stop(sprintf("Error in resilience classification: %s", e$message))
  })
  
  # Step 8: Model Diagnostics
  message("\n8. Computing model diagnostics...")
  
  model_diagnostics_results <- model_diagnostics(cacs_model, percentile_results)
  
  # Step 9: Combine all results
  message("\n9. Combining results...")
  
  final_data <- merge(modeling_data, classification_results, by = "row.names", all = TRUE)
  rownames(final_data) <- final_data$Row.names
  final_data$Row.names <- NULL
  
  # Create results object
  results <- list(
    # Data
    prepared_data = prepared_data,
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
      cacs_col = cacs_col,
      risk_scores = risk_scores,
      risk_region = risk_region,
      percentile_thresholds = percentile_thresholds,
      min_scores = min_scores,
      cholesterol_unit = cholesterol_unit
    )
  )
  
  # Step 10: Generate plots if requested
  if (include_plots) {
    message("\n10. Generating diagnostic plots...")
    
    tryCatch({
      results$plots <- list()
      
      # Main resilience plot
      results$plots$cacs_vs_risk <- plot_cacs_vs_risk(
        final_data,
        cacs_col = cacs_col,
        risk_score_col = "average_norm_score",
        class_col = "resilience_class"
      )
      
      # Risk distribution plot
      results$plots$risk_distribution <- plot_risk_distribution(
        final_data,
        risk_score_col = "average_norm_score",
        class_col = "resilience_class"
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