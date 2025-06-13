# Tests for visualization functions

test_that("plot_percentile_distribution works correctly", {
  # Create test resilience results
  test_results <- data.frame(
    cacs_percentile = c(5, 15, 25, 45, 55, 75, 85, 95)
  )
  rownames(test_results) <- paste0("S", 1:8)
  
  # Test that function runs without error
  expect_no_error(plot_percentile_distribution(test_results))
})

test_that("plot_percentile_distribution validates input", {
  # Test with invalid input
  expect_error(plot_percentile_distribution("not_data_frame"), 
               "Required columns missing from data")
  
  # Test with missing required columns
  bad_data <- data.frame(other_col = 1:5)
  expect_error(plot_percentile_distribution(bad_data), 
               "Required columns missing from data")
})

test_that("plot_cacs_vs_risk works correctly", {
  # Create test data
  test_data <- data.frame(
    cacs = c(0, 5, 15, 45, 100, 250, 500),
    average_norm_score = rnorm(7),
    classification = c("resilient", "reference", "reference", 
                      "reference", "susceptible", "susceptible", "susceptible")
  )
  rownames(test_data) <- paste0("S", 1:7)
  
  # Test that function runs without error
  expect_no_error(plot_cacs_vs_risk(test_data))
})

test_that("plot_cacs_vs_risk validates input", {
  # Test with invalid input
  expect_error(plot_cacs_vs_risk("not_data_frame"), 
               "Required columns missing from data")
  
  # Test with missing required columns
  bad_data <- data.frame(other_col = 1:5)
  expect_error(plot_cacs_vs_risk(bad_data), 
               "Required columns missing from data")
})

test_that("plot_risk_distribution works correctly", {
  # Create test ensemble results
  test_ensemble <- data.frame(
    average_norm_score = rnorm(20),
    classification = sample(c("resilient", "reference", "susceptible"), 20, replace = TRUE)
  )
  rownames(test_ensemble) <- paste0("S", 1:20)
  
  # Test that function runs without error
  expect_no_error(plot_risk_distribution(test_ensemble))
})

test_that("plot_risk_distribution validates input", {
  # Test with invalid input
  expect_error(plot_risk_distribution("not_data_frame"), 
               "Required columns missing from data")
  
  # Test with missing score columns
  bad_data <- data.frame(other_col = 1:5)
  expect_error(plot_risk_distribution(bad_data), 
               "Required columns missing from data")
})

test_that("plot_resilience_summary works correctly", {
  # Create comprehensive test results
  test_results <- data.frame(
    cacs = c(0, 5, 15, 45, 100, 250),
    cacs_percentile = c(5, 25, 45, 65, 85, 95),
    classification = c("resilient", "reference", "reference", 
                      "reference", "susceptible", "susceptible"),
    average_norm_score = rnorm(6)
  )
  rownames(test_results) <- paste0("S", 1:6)
  
  # Test that function runs without error
  expect_no_error(plot_resilience_summary(test_results))
})

test_that("visualization functions handle missing data", {
  # Create test data with missing values
  test_data <- data.frame(
    cacs = c(0, NA, 15, 45),
    cacs_percentile = c(5, 25, NA, 65),
    classification = c("resilient", "reference", NA, "reference"),
    average_norm_score = c(0.5, NA, -0.3, 0.8)
  )
  rownames(test_data) <- paste0("S", 1:4)
  
  # Functions should handle missing data gracefully
  expect_no_error(plot_percentile_distribution(test_data))
  expect_no_error(plot_cacs_vs_risk(test_data))
})

test_that("model diagnostics plotting works", {
  # This requires an actual model, so just test that the function exists
  expect_true(exists("plot_model_diagnostics"))
  expect_true(is.function(plot_model_diagnostics))
})