#' Fit zero-inflated negative binomial model for CACS prediction
#'
#' Fits a zero-inflated negative binomial regression model to predict CACS based on
#' ensemble risk scores, following the BioHEART resilience methodology.
#'
#' @param data Data frame containing CACS and ensemble risk scores
#' @param cacs_col Name of CACS column (default: "cacs")
#' @param risk_score_col Name of ensemble risk score column (default: "average_norm_score")
#' @param scale_factor Factor to scale CACS for model fitting (default: 100)
#' @param ... Additional arguments passed to zeroinfl()
#' @return zeroinfl model object with additional attributes
#' @export
#' @examples
#' \dontrun{
#' # Fit CACS model using ensemble risk scores
#' model <- fit_cacs_model(combined_data, 
#'                        cacs_col = "cacs", 
#'                        risk_score_col = "average_norm_score")
#' }
fit_cacs_model <- function(data, 
                          cacs_col = "cacs",
                          risk_score_col = "average_norm_score",
                          scale_factor = 100,
                          ...) {
  
  # Validate inputs
  if (!inherits(data, "data.frame")) {
    stop("data must be a data frame")
  }
  
  required_cols <- c(cacs_col, risk_score_col)
  validate_columns(data, required_cols)
  
  # Extract variables for modeling
  cacs_values <- data[[cacs_col]]
  risk_scores <- data[[risk_score_col]]
  
  # Check for missing values
  complete_cases <- complete.cases(cacs_values, risk_scores)
  n_missing <- sum(!complete_cases)
  
  if (n_missing > 0) {
    warning(sprintf("Removing %d rows with missing CACS or risk scores", n_missing))
    cacs_values <- cacs_values[complete_cases]
    risk_scores <- risk_scores[complete_cases]
  }
  
  if (length(cacs_values) == 0) {
    stop("No complete cases available for modeling")
  }
  
  # Validate CACS values
  if (any(cacs_values < 0, na.rm = TRUE)) {
    stop("CACS values must be non-negative")
  }
  
  # Scale CACS for better model fitting (commonly done in the literature)
  scaled_cacs <- cacs_values * scale_factor
  
  # Check for integer overflow after scaling
  if (any(scaled_cacs > .Machine$integer.max, na.rm = TRUE)) {
    warning("Some scaled CACS values are very large, consider reducing scale_factor")
  }
  
  # Convert to integer for count model
  scaled_cacs_int <- as.integer(round(scaled_cacs))
  
  # Create model data frame
  model_data <- data.frame(
    cacs_scaled = scaled_cacs_int,
    risk_score = risk_scores
  )
  
  # Fit zero-inflated negative binomial model
  # Formula: count component and zero-inflation component both use risk score
  tryCatch({
    message("Fitting zero-inflated negative binomial model...")
    
    model <- pscl::zeroinfl(
      cacs_scaled ~ risk_score | risk_score,
      data = model_data,
      dist = "negbin",
      ...
    )
    
    # Add custom attributes
    attr(model, "cacs_column") <- cacs_col
    attr(model, "risk_score_column") <- risk_score_col
    attr(model, "scale_factor") <- scale_factor
    attr(model, "n_observations") <- length(cacs_values)
    attr(model, "n_missing_removed") <- n_missing
    attr(model, "original_data_rows") <- rownames(data)[complete_cases]
    attr(model, "fit_date") <- Sys.Date()
    
    class(model) <- c("cacs_resilience_model", class(model))
    
    message(sprintf("Model fitted successfully with %d observations", length(cacs_values)))
    
    return(model)
    
  }, error = function(e) {
    stop(sprintf("Error fitting zero-inflated model: %s", e$message))
  })
}

#' Calculate risk-adjusted CACS percentiles (Calcium Vulnerability Score)
#'
#' Calculates the percentile of observed CACS within the predicted distribution
#' based on the zero-inflated negative binomial model.
#'
#' @param model Fitted CACS model from fit_cacs_model()
#' @param data Data frame containing CACS and risk scores (can be new data)
#' @param cacs_col Name of CACS column
#' @param risk_score_col Name of risk score column
#' @return Data frame with percentiles and predictions (preserves row names)
#' @export
#' @examples
#' \dontrun{
#' # Calculate percentiles for the same data used to fit model
#' percentiles <- calculate_cacs_percentiles(model, combined_data)
#' 
#' # Calculate percentiles for new data
#' new_percentiles <- calculate_cacs_percentiles(model, new_data)
#' }
calculate_cacs_percentiles <- function(model, 
                                      data,
                                      cacs_col = NULL,
                                      risk_score_col = NULL) {
  
  # Extract column names from model if not provided
  if (is.null(cacs_col)) {
    cacs_col <- attr(model, "cacs_column") %||% "cacs"
  }
  if (is.null(risk_score_col)) {
    risk_score_col <- attr(model, "risk_score_column") %||% "average_norm_score"
  }
  
  # Validate inputs
  required_cols <- c(cacs_col, risk_score_col)
  validate_columns(data, required_cols)
  
  # Extract model attributes
  scale_factor <- attr(model, "scale_factor") %||% 100
  
  # Prepare data
  cacs_values <- data[[cacs_col]]
  risk_scores <- data[[risk_score_col]]
  
  # Initialize results preserving row names
  results <- data.frame(row.names = rownames(data), stringsAsFactors = FALSE)
  
  # Check for complete cases
  complete_cases <- complete.cases(cacs_values, risk_scores)
  
  # Initialize all results as NA
  results$cacs_original <- cacs_values
  results$risk_score <- risk_scores
  results$cacs_expected_count <- NA
  results$cacs_zero_prob <- NA
  results$cacs_expected_mean <- NA
  results$cacs_percentile <- NA
  
  if (sum(complete_cases) == 0) {
    warning("No complete cases found for percentile calculation")
    return(results)
  }
  
  # Create prediction data frame for complete cases
  pred_data <- data.frame(
    risk_score = risk_scores[complete_cases]
  )
  
  tryCatch({
    # Get model predictions for complete cases
    # Count predictions (from negative binomial component)
    count_pred <- predict(model, newdata = pred_data, type = "count")
    
    # Zero probability predictions (from zero-inflation component)  
    zero_prob <- predict(model, newdata = pred_data, type = "zero")
    
    # Overall mean predictions (combined model)
    mean_pred <- predict(model, newdata = pred_data, type = "response")
    
    # Store predictions (unscaled)
    results$cacs_expected_count[complete_cases] <- count_pred / scale_factor
    results$cacs_zero_prob[complete_cases] <- zero_prob
    results$cacs_expected_mean[complete_cases] <- mean_pred / scale_factor
    
    # Calculate percentiles for complete cases
    scaled_cacs <- cacs_values[complete_cases] * scale_factor
    
    # Calculate percentiles using the zero-inflated negative binomial distribution
    percentiles <- calculate_zinb_percentiles(
      observed_counts = scaled_cacs,
      mu = count_pred,  # mu parameter for negative binomial
      size = model$theta,  # size parameter (theta) for negative binomial
      zero_prob = zero_prob
    )
    
    results$cacs_percentile[complete_cases] <- percentiles
    
    # Warn about missing cases
    n_missing <- sum(!complete_cases)
    if (n_missing > 0) {
      warning(sprintf("%d rows with missing data will have NA percentiles", n_missing))
    }
    
  }, error = function(e) {
    warning(sprintf("Error calculating percentiles: %s", e$message))
  })
  
  # Add attributes
  attr(results, "model_used") <- deparse(substitute(model))
  attr(results, "scale_factor") <- scale_factor
  attr(results, "calculation_date") <- Sys.Date()
  attr(results, "n_complete") <- sum(complete_cases)
  
  return(results)
}

#' Calculate percentiles for zero-inflated negative binomial distribution
#'
#' @param observed_counts Vector of observed count values
#' @param mu Mean parameter for negative binomial component
#' @param size Size parameter for negative binomial component  
#' @param zero_prob Probability of structural zero
#' @return Vector of percentiles
#' @keywords internal
calculate_zinb_percentiles <- function(observed_counts, mu, size, zero_prob) {
  
  percentiles <- numeric(length(observed_counts))
  
  for (i in seq_along(observed_counts)) {
    obs_count <- observed_counts[i]
    
    if (is.na(obs_count) || is.na(mu[i]) || is.na(zero_prob[i])) {
      percentiles[i] <- NA
      next
    }
    
    if (obs_count == 0) {
      # For zero observations, percentile is the average of P(X <= 0) and P(X < 0)
      # P(X < 0) = 0, P(X <= 0) includes both structural zeros and sampling zeros
      prob_zero_total <- zero_prob[i] + (1 - zero_prob[i]) * stats::dnbinom(0, size = size, mu = mu[i])
      percentiles[i] <- 0.5 * prob_zero_total
      
    } else {
      # For non-zero observations, percentile is average of P(X <= obs) and P(X < obs)
      # Both include the structural zero probability plus the negative binomial probabilities
      prob_leq <- zero_prob[i] + (1 - zero_prob[i]) * stats::pnbinom(obs_count, size = size, mu = mu[i])
      prob_less <- zero_prob[i] + (1 - zero_prob[i]) * stats::pnbinom(obs_count - 1, size = size, mu = mu[i])
      
      percentiles[i] <- 0.5 * (prob_leq + prob_less)
    }
  }
  
  return(percentiles)
}

#' Classify resilience based on CACS percentiles
#'
#' Classifies subjects as resilient, susceptible, reference, or other based on
#' their risk-adjusted CACS percentiles.
#'
#' @param percentile_data Data frame from calculate_cacs_percentiles()
#' @param thresholds Named vector of classification thresholds
#' @param percentile_col Name of percentile column (default: "cacs_percentile")
#' @return Data frame with classification results (preserves row names)
#' @export
#' @examples
#' \dontrun{
#' # Use default thresholds
#' classifications <- classify_resilience(percentile_data)
#' 
#' # Use custom thresholds
#' custom_thresholds <- c(resilient = 15, reference_low = 35, 
#'                       reference_high = 65, susceptible = 85)
#' classifications <- classify_resilience(percentile_data, custom_thresholds)
#' }
classify_resilience <- function(percentile_data,
                               thresholds = c(resilient = 20, reference_low = 40, 
                                            reference_high = 60, susceptible = 80),
                               percentile_col = "cacs_percentile") {
  
  # Validate inputs
  if (!inherits(percentile_data, "data.frame")) {
    stop("percentile_data must be a data frame")
  }
  
  if (!percentile_col %in% names(percentile_data)) {
    stop(sprintf("Column '%s' not found in data", percentile_col))
  }
  
  # Validate thresholds
  required_thresholds <- c("resilient", "reference_low", "reference_high", "susceptible")
  missing_thresholds <- setdiff(required_thresholds, names(thresholds))
  if (length(missing_thresholds) > 0) {
    stop(sprintf("Missing threshold values: %s", paste(missing_thresholds, collapse = ", ")))
  }
  
  # Extract percentiles
  percentiles <- percentile_data[[percentile_col]]
  
  # Initialize results preserving row names
  results <- data.frame(row.names = rownames(percentile_data), stringsAsFactors = FALSE)
  
  # Copy original percentile data
  results[[percentile_col]] <- percentiles
  
  # Classify based on thresholds (convert percentiles to 0-100 scale if needed)
  percentiles_pct <- percentiles * 100  # Convert to percentage if in 0-1 scale
  
  # Apply classification rules
  results$resilience_class <- ifelse(
    is.na(percentiles_pct), "missing",
    ifelse(percentiles_pct < thresholds["resilient"], "resilient",
    ifelse(percentiles_pct > thresholds["susceptible"], "susceptible", 
    ifelse(percentiles_pct >= thresholds["reference_low"] & 
           percentiles_pct <= thresholds["reference_high"], "reference", "other")))
  )
  
  # Convert to factor with defined levels
  results$resilience_class <- factor(
    results$resilience_class,
    levels = c("resilient", "reference", "susceptible", "other", "missing"),
    ordered = FALSE
  )
  
  # Add confidence metrics
  results$distance_to_resilient <- abs(percentiles_pct - thresholds["resilient"])
  results$distance_to_susceptible <- abs(percentiles_pct - thresholds["susceptible"])
  results$distance_to_reference <- pmin(
    abs(percentiles_pct - thresholds["reference_low"]),
    abs(percentiles_pct - thresholds["reference_high"])
  )
  
  # Add classification summary
  class_summary <- table(results$resilience_class, useNA = "ifany")
  
  message("Resilience Classification Summary:")
  message(sprintf("  Resilient: %d (%.1f%%)", 
                 class_summary["resilient"], 
                 100 * class_summary["resilient"] / nrow(results)))
  message(sprintf("  Reference: %d (%.1f%%)", 
                 class_summary["reference"], 
                 100 * class_summary["reference"] / nrow(results)))
  message(sprintf("  Susceptible: %d (%.1f%%)", 
                 class_summary["susceptible"], 
                 100 * class_summary["susceptible"] / nrow(results)))
  message(sprintf("  Other: %d (%.1f%%)", 
                 class_summary["other"], 
                 100 * class_summary["other"] / nrow(results)))
  message(sprintf("  Missing: %d (%.1f%%)", 
                 class_summary["missing"], 
                 100 * class_summary["missing"] / nrow(results)))
  
  # Add attributes
  attr(results, "thresholds_used") <- thresholds
  attr(results, "classification_date") <- Sys.Date()
  attr(results, "n_total") <- nrow(results)
  attr(results, "class_summary") <- class_summary
  
  return(results)
}

#' Get model diagnostics and goodness of fit statistics
#'
#' @param model Fitted CACS model
#' @param percentile_data Optional percentile data for additional diagnostics
#' @return List with diagnostic information
#' @export
model_diagnostics <- function(model, percentile_data = NULL) {
  
  if (!inherits(model, "zeroinfl")) {
    stop("model must be a zeroinfl object")
  }
  
  # Basic model summary
  model_summary <- summary(model)
  
  # Model fit statistics
  fit_stats <- list(
    log_likelihood = stats::logLik(model),
    aic = stats::AIC(model),
    bic = stats::BIC(model),
    n_observations = nobs(model),
    theta = model$theta,  # Overdispersion parameter
    converged = model$converged
  )
  
  # Predicted vs observed for fitted data
  fitted_mean <- stats::fitted(model)
  observed <- model$y
  
  # Calculate correlation between predicted and observed
  pred_obs_cor <- stats::cor(fitted_mean, observed, use = "complete.obs")
  
  diagnostics <- list(
    model_summary = model_summary,
    fit_statistics = fit_stats,
    predicted_observed_correlation = pred_obs_cor,
    fitted_values = fitted_mean,
    observed_values = observed
  )
  
  # Add percentile-based diagnostics if provided
  if (!is.null(percentile_data)) {
    if ("cacs_percentile" %in% names(percentile_data)) {
      percentiles <- percentile_data$cacs_percentile
      
      # Check if percentiles are uniformly distributed (good model fit)
      ks_test <- stats::ks.test(percentiles[!is.na(percentiles)], "punif")
      
      diagnostics$percentile_uniformity_test <- ks_test
      diagnostics$percentile_summary <- summary(percentiles)
    }
  }
  
  class(diagnostics) <- c("cacs_model_diagnostics", "list")
  return(diagnostics)
}

#' Print method for model diagnostics
#'
#' @param x cacs_model_diagnostics object
#' @param ... Additional arguments (ignored)
#' @export
print.cacs_model_diagnostics <- function(x, ...) {
  cat("CACS Resilience Model Diagnostics\n")
  cat("==================================\n\n")
  
  cat("Model Fit Statistics:\n")
  cat(sprintf("  Log-likelihood: %.2f\n", x$fit_statistics$log_likelihood))
  cat(sprintf("  AIC: %.2f\n", x$fit_statistics$aic))
  cat(sprintf("  BIC: %.2f\n", x$fit_statistics$bic))
  cat(sprintf("  N observations: %d\n", x$fit_statistics$n_observations))
  cat(sprintf("  Theta (overdispersion): %.4f\n", x$fit_statistics$theta))
  cat(sprintf("  Converged: %s\n", x$fit_statistics$converged))
  
  cat(sprintf("\nPredicted vs Observed Correlation: %.3f\n", x$predicted_observed_correlation))
  
  if (!is.null(x$percentile_uniformity_test)) {
    cat("\nPercentile Distribution Test (should be uniform):\n")
    cat(sprintf("  Kolmogorov-Smirnov test p-value: %.4f\n", x$percentile_uniformity_test$p.value))
    if (x$percentile_uniformity_test$p.value > 0.05) {
      cat("  ✓ Percentiles appear uniformly distributed (good model fit)\n")
    } else {
      cat("  ⚠ Percentiles may not be uniformly distributed (check model fit)\n")
    }
  }
  
  invisible(x)
}