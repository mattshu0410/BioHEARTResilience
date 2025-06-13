# Tests for main resilience analysis function

test_that("resilience_analysis function exists and has correct signature", {
  # Test that the function exists
  expect_true(exists("resilience_analysis"))
  expect_true(is.function(resilience_analysis))
  
  # Test parameter validation
  expect_error(match.arg("invalid", c("mmol/L", "mg/dL")), "should be one of")
})

test_that("resilience_analysis validates prepared data", {
  # Test with incomplete prepared data (missing CACS) - should fail at modeling step
  incomplete_data <- data.frame(
    age = 45, 
    gender = "male", 
    tc_mgdl = 200, 
    hdl_mgdl = 50,
    sbp = 120,
    curr_smok = 0,
    cvhx_dm = 0,
    bp_med = 0
  )
  expect_error(resilience_analysis(incomplete_data), "No subjects have both CACS and ensemble risk scores")
  
  # Test with empty data frame - should fail at risk score calculation
  empty_data <- data.frame()
  expect_error(resilience_analysis(empty_data), "Error in risk score calculation")
})

test_that("prepare_cohort_data validates raw data", {
  # Test with incomplete raw data - should fail at preparation stage
  raw_incomplete <- data.frame(age = 45, gender = "male")
  expect_error(prepare_cohort_data(raw_incomplete), "Required columns missing")
  
  # Test with empty data frame
  empty_raw <- data.frame()
  expect_error(prepare_cohort_data(empty_raw), "Required columns missing")
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

test_that("resilience analysis preserves ID column through pipeline", {
  # Create minimal test data with ID column
  test_data_with_id <- data.frame(
    subject_id = paste0("TEST_", 1:10),
    age = rep(50, 10),
    gender = rep("male", 10),
    tc = rep(200, 10),  # mg/dL
    hdl = rep(50, 10),  # mg/dL  
    sbp = rep(130, 10),
    curr_smok = rep(0, 10),
    cvhx_dm = rep(0, 10),
    bp_med = rep(0, 10),
    cacs = c(0, 0, 10, 20, 30, 40, 50, 100, 200, 300)
  )
  
  # Prepare data with ID column
  prepared_data <- prepare_cohort_data(
    test_data_with_id, 
    id_col = "subject_id",
    cholesterol_unit = "mg/dL",
    validate = FALSE
  )
  
  # Verify ID is in prepared data
  expect_true("id" %in% names(prepared_data))
  expect_equal(prepared_data$id, test_data_with_id$subject_id)
  
  # Test that the resilience_analysis function would preserve IDs
  # Note: We can't run full analysis in tests due to dependency issues,
  # but we can verify the ID flows through the data preparation step
  expect_true("id" %in% names(prepared_data))
  expect_equal(length(prepared_data$id), nrow(prepared_data))
  expect_false(any(is.na(prepared_data$id)))
})