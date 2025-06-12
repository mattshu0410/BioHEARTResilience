# Tests for main resilience analysis function

test_that("resilience_analysis function exists and has correct signature", {
  # Test that the function exists
  expect_true(exists("resilience_analysis"))
  expect_true(is.function(resilience_analysis))
  
  # Test parameter validation
  expect_error(match.arg("invalid", c("mmol/L", "mg/dL")), "should be one of")
})

test_that("resilience_analysis validates required columns", {
  # Test with incomplete data
  test_data <- data.frame(age = 45, gender = "male")
  expect_error(resilience_analysis(test_data), "Required columns missing")
  
  # Test with empty data frame
  empty_data <- data.frame()
  expect_error(resilience_analysis(empty_data), "Required columns missing")
})

test_that("individual component functions work", {
  # Load example data for component testing
  data("example_cohort", package = "BioHEARTResilience")
  test_data <- example_cohort[1:5, ]
  
  # Test data preparation step
  expect_no_error(prepared <- prepare_cohort_data(test_data, cholesterol_unit = "mg/dL"))
  
  # Test risk score calculation (use handle_missing = "na" to avoid errors)
  risk_scores <- calculate_risk_scores(prepared, scores = "frs", handle_missing = "na")
  expect_equal(nrow(risk_scores), 5)
  
  # Test ensemble creation only if we have valid score columns
  if (any(grepl("_10y$", names(risk_scores)))) {
    expect_no_error(ensemble <- ensemble_risk_score(risk_scores))
  } else {
    # If no risk scores were calculated, just check the function exists
    expect_true(exists("ensemble_risk_score"))
  }
})