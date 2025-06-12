# Tests for resilience modeling functions

test_that("integrated cacs model and percentile calculation works", {
  # Create realistic test data with zero-inflated structure
  set.seed(42)
  n <- 50  # Smaller for testing
  test_data <- data.frame(
    age = runif(n, 40, 80),
    gender = sample(c("male", "female"), n, replace = TRUE),
    curr_smok = sample(0:1, n, replace = TRUE),
    cvhx_dm = sample(0:1, n, replace = TRUE),
    average_norm_score = rnorm(n, 0, 1)
  )
  
  # Generate zero-inflated CACS data
  prob_zero <- plogis(-2 + 0.5 * test_data$age/10 + 0.3 * (test_data$gender == "male"))
  zero_mask <- runif(n) < prob_zero
  cacs_nonzero <- rnbinom(n, size = 2, mu = exp(1 + 0.1 * test_data$age + 0.5 * test_data$average_norm_score))
  test_data$cacs <- ifelse(zero_mask, 0, cacs_nonzero)
  
  rownames(test_data) <- paste0("S", 1:n)
  
  # Test integrated workflow: fit model then calculate percentiles
  model <- fit_cacs_model(test_data)
  percentiles <- calculate_cacs_percentiles(model, test_data)
  
  expect_equal(nrow(percentiles), n)
  expect_equal(rownames(percentiles), rownames(test_data))
  expect_true("cacs_percentile" %in% names(percentiles))
  
  # Check percentile range
  expect_true(all(percentiles$cacs_percentile >= 0 & percentiles$cacs_percentile <= 100, na.rm = TRUE))
})

test_that("classify_resilience handles data correctly", {
  # Create test percentile data in 0-1 scale (as expected by the function)
  test_data <- data.frame(
    cacs_percentile = c(0.05, 0.15, 0.45, 0.55, 0.85, 0.95),
    row.names = paste0("S", 1:6)
  )
  
  # Test classification
  result <- classify_resilience(test_data)
  
  expect_equal(nrow(result), 6)
  expect_true("resilience_class" %in% names(result))
  
  # Check specific classifications (function returns factors)
  expect_equal(as.character(result$resilience_class[1]), "resilient")  # 5th percentile
  expect_equal(as.character(result$resilience_class[3]), "reference")  # 45th percentile
  expect_equal(as.character(result$resilience_class[5]), "susceptible")  # 85th percentile
})

test_that("classify_resilience works with data frame input", {
  # Test with different percentile values as data frame in 0-1 scale
  # Default thresholds: resilient < 20, reference 40-60, susceptible > 80
  test_data <- data.frame(
    cacs_percentile = c(0.05, 0.15, 0.45, 0.55, 0.85, 0.95)
  )
  
  result <- classify_resilience(test_data)
  expected <- c("resilient", "resilient", "reference", "reference", 
                "susceptible", "susceptible")
  
  expect_equal(as.character(result$resilience_class), expected)
  
  # Test with custom thresholds
  custom_thresholds <- c(resilient = 10, reference_low = 20, reference_high = 80, susceptible = 90)
  result_custom <- classify_resilience(test_data, thresholds = custom_thresholds)
  
  expect_true("resilience_class" %in% names(result_custom))
  expect_equal(as.character(result_custom$resilience_class[1]), "resilient")  # 5th percentile
  expect_equal(as.character(result_custom$resilience_class[6]), "susceptible")  # 95th percentile
  
  # Test with NA values - use percentiles that fall clearly in expected categories
  test_data_na <- data.frame(cacs_percentile = c(0.05, NA, 0.45, 0.85))
  result_na <- classify_resilience(test_data_na)
  expect_equal(as.character(result_na$resilience_class[2]), "missing")  # NA becomes "missing"
  expect_equal(as.character(result_na$resilience_class[c(1, 3, 4)]), c("resilient", "reference", "susceptible"))
})

test_that("classify_resilience validates input", {
  # Test with invalid data frame
  expect_error(classify_resilience("not_data_frame"), "percentile_data must be a data frame")
  
  # Test with missing required thresholds
  bad_thresholds <- c(resilient = 20, susceptible = 80)  # Missing reference thresholds
  test_data <- data.frame(cacs_percentile = c(10, 50, 90))
  expect_error(classify_resilience(test_data, thresholds = bad_thresholds), 
               "Missing threshold values")
})

test_that("fit_cacs_model works correctly", {
  # Create test data for model fitting
  set.seed(42)
  n <- 50
  test_data <- data.frame(
    age = runif(n, 40, 80),
    gender = sample(c("male", "female"), n, replace = TRUE),
    curr_smok = sample(0:1, n, replace = TRUE),
    cvhx_dm = sample(0:1, n, replace = TRUE),
    average_norm_score = rnorm(n, 0, 1)
  )
  
  # Generate CACS with some zeros
  prob_zero <- 0.3
  cacs_values <- ifelse(runif(n) < prob_zero, 0, 
                       rnbinom(n, size = 2, mu = exp(1 + 0.1 * test_data$age)))
  test_data$cacs <- cacs_values
  
  # Test model fitting
  model <- fit_cacs_model(test_data)
  
  expect_true(inherits(model, "zeroinfl"))
  expect_true(all(c("coefficients", "fitted.values") %in% names(model)))
})

test_that("calculate_cacs_percentiles works correctly", {
  # Create simple test model and data
  set.seed(42)
  n <- 30
  test_data <- data.frame(
    age = runif(n, 40, 80),
    gender = sample(c("male", "female"), n, replace = TRUE),
    curr_smok = sample(0:1, n, replace = TRUE),
    cvhx_dm = sample(0:1, n, replace = TRUE),
    average_norm_score = rnorm(n, 0, 1),
    cacs = sample(c(0, 5, 15, 45, 100, 250), n, replace = TRUE)
  )
  
  # Fit model
  model <- fit_cacs_model(test_data)
  
  # Calculate percentiles
  percentiles <- calculate_cacs_percentiles(model, test_data)
  
  expect_equal(nrow(percentiles), n)
  expect_true("cacs_percentile" %in% names(percentiles))
  expect_true(all(percentiles$cacs_percentile >= 0 & percentiles$cacs_percentile <= 100, na.rm = TRUE))
})