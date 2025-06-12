## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)


## ----eval=FALSE---------------------------------------------------------------
## # Install from GitHub (when available)
## devtools::install_github("yourusername/BioHEARTResilience")
## 
## # Load the package
## library(BioHEARTResilience)


## ----setup--------------------------------------------------------------------
library(BioHEARTResilience)


## -----------------------------------------------------------------------------
# Load example data
data("example_cohort")

# Examine the data structure
str(example_cohort)


## -----------------------------------------------------------------------------
head(example_cohort)


## -----------------------------------------------------------------------------
summary(example_cohort[c("age", "tc", "hdl", "sbp", "cacs")])


## ----eval=FALSE---------------------------------------------------------------
## # Run complete analysis
## results <- resilience_analysis(
##   data = example_cohort,
##   cacs_col = "cacs",
##   cholesterol_unit = "mmol/L",
##   include_plots = TRUE
## )


## ----echo=FALSE---------------------------------------------------------------
# For vignette, run without plots to avoid potential issues
results <- resilience_analysis(
  data = example_cohort,
  cacs_col = "cacs", 
  cholesterol_unit = "mmol/L",
  include_plots = FALSE
)


## -----------------------------------------------------------------------------
# View the results summary
print(results)


## -----------------------------------------------------------------------------
# View resilience classifications
table(results$classifications$resilience_class)


## -----------------------------------------------------------------------------
# Look at classification details for first 10 subjects
head(results$classifications[c("cacs_original", "cacs_percentile", "resilience_class")], 10)


## -----------------------------------------------------------------------------
# Check risk score completion
head(results$risk_summary$summary)


## -----------------------------------------------------------------------------
# Model diagnostics
results$model_diagnostics$fit_statistics


## -----------------------------------------------------------------------------
# Prepare data with validation
prepared_data <- prepare_cohort_data(
  example_cohort,
  cholesterol_unit = "mmol/L"
)

cat("Prepared data dimensions:", dim(prepared_data), "\n")


## -----------------------------------------------------------------------------
# Calculate individual risk scores
risk_scores <- calculate_risk_scores(
  prepared_data,
  scores = c("frs", "ascvd", "mesa", "score2")
)

# Summarize risk scores
risk_summary <- summarize_risk_scores(risk_scores)
print(risk_summary)


## -----------------------------------------------------------------------------
# Create ensemble risk score using ordered quantile normalization
ensemble_scores <- ensemble_risk_score(risk_scores)

# Check ensemble results
cat("Ensemble scores created for", sum(!is.na(ensemble_scores$average_norm_score)), "subjects\n")
head(ensemble_scores[c("average_norm_score", "n_scores_used")])


## -----------------------------------------------------------------------------
# Combine data for modeling
modeling_data <- merge(prepared_data, ensemble_scores, by = "row.names")
rownames(modeling_data) <- modeling_data$Row.names
modeling_data$Row.names <- NULL

# Fit zero-inflated negative binomial model
cacs_model <- fit_cacs_model(
  modeling_data,
  cacs_col = "cacs",
  risk_score_col = "average_norm_score"
)

# Check model summary
summary(cacs_model)


## -----------------------------------------------------------------------------
# Calculate the "Calcium Vulnerability Score"
percentiles <- calculate_cacs_percentiles(
  cacs_model,
  modeling_data
)

# Examine percentile distribution
summary(percentiles$cacs_percentile)


## -----------------------------------------------------------------------------
# Classify based on percentile thresholds
classifications <- classify_resilience(percentiles)

# View classification distribution
table(classifications$resilience_class)


## ----eval=FALSE---------------------------------------------------------------
## # Combine final data
## final_data <- merge(modeling_data, classifications, by = "row.names")
## rownames(final_data) <- final_data$Row.names
## 
## # Plot CACS vs risk score colored by classification
## plot_cacs_vs_risk(
##   final_data,
##   cacs_col = "cacs",
##   risk_score_col = "average_norm_score",
##   class_col = "resilience_class"
## )


## ----eval=FALSE---------------------------------------------------------------
## # Plot risk score distributions by class
## plot_risk_distribution(
##   final_data,
##   risk_score_col = "average_norm_score",
##   class_col = "resilience_class"
## )


## ----eval=FALSE---------------------------------------------------------------
## # Generate model diagnostic plots
## diagnostic_plots <- plot_model_diagnostics(cacs_model, percentiles)


## ----eval=FALSE---------------------------------------------------------------
## # Use only specific risk scores
## results_custom <- resilience_analysis(
##   example_cohort,
##   risk_scores = c("frs", "ascvd"),  # Only FRS and ASCVD
##   include_plots = FALSE
## )


## ----eval=FALSE---------------------------------------------------------------
## # Use custom percentile thresholds
## results_custom_thresh <- resilience_analysis(
##   example_cohort,
##   percentile_thresholds = c(
##     resilient = 15,        # More stringent resilient threshold
##     reference_low = 35,
##     reference_high = 65,
##     susceptible = 85       # More stringent susceptible threshold
##   ),
##   include_plots = FALSE
## )


## ----eval=FALSE---------------------------------------------------------------
## # For European populations at moderate risk
## results_moderate_risk <- resilience_analysis(
##   example_cohort,
##   risk_region = "Moderate",  # For SCORE2 calculation
##   include_plots = FALSE
## )


## -----------------------------------------------------------------------------
# Check missing data patterns in original data
missing_summary <- check_missing_values(example_cohort, verbose = FALSE)
if (nrow(missing_summary) > 0) {
  print(missing_summary)
} else {
  cat("No missing data found in example dataset\n")
}


## -----------------------------------------------------------------------------
# Get information about available risk scores
risk_score_info()


## -----------------------------------------------------------------------------
# Check which subjects are valid for each risk score
valid_subjects <- validate_risk_data(
  prepared_data, 
  scores = c("frs", "ascvd", "mesa", "score2")
)

cat("Valid subjects for all scores:", sum(valid_subjects), "out of", length(valid_subjects), "\n")


## ----eval=FALSE---------------------------------------------------------------
## # Calculate individual risk scores manually
## frs_scores <- CVrisk::ascvd_10y_frs(
##   gender = prepared_data$gender,
##   age = prepared_data$age,
##   hdl = prepared_data$hdl_mgdl,
##   totchol = prepared_data$tc_mgdl,
##   sbp = prepared_data$sbp,
##   bp_med = prepared_data$bp_med,
##   smoker = prepared_data$curr_smok,
##   diabetes = prepared_data$cvhx_dm
## )


## ----eval=FALSE---------------------------------------------------------------
## # View function help
## ?resilience_analysis
## ?calculate_risk_scores
## ?ensemble_risk_score
## 
## # View this vignette
## vignette("introduction", package = "BioHEARTResilience")

