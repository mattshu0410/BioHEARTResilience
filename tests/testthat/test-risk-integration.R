# Tests for risk score integration functions

test_that("risk_score_info works correctly", {
  # Test getting all risk score info
  all_info <- risk_score_info()
  expect_equal(nrow(all_info), 4)
  expect_true(all(c("frs", "ascvd", "mesa", "score2") %in% all_info$score))
  expect_true(all(c("full_name", "outcome", "age_range") %in% names(all_info)))
  
  # Test getting specific score info
  frs_info <- risk_score_info("frs")
  expect_equal(nrow(frs_info), 1)
  expect_equal(frs_info$score, "frs")
  
  # Test invalid score
  expect_error(risk_score_info("invalid_score"), "Unknown score")
})

test_that("calculate_risk_scores handles missing data", {
  # Create test data with some missing values
  test_data <- data.frame(
    age = c(45, 55, NA),  # One missing age
    gender = c("male", "female", "male"),
    tc_mgdl = c(200, 220, 180),
    hdl_mgdl = c(40, 50, 45),
    sbp = c(130, 140, 120),
    curr_smok = c(0, 1, 0),
    cvhx_dm = c(0, 0, 1),
    bp_med = c(0, 1, 1),
    ethnicity_ascvd = c("white", "white", "aa"),
    ethnicity_mesa = c("white", "white", "aa")
  )
  rownames(test_data) <- paste0("S", 1:3)
  
  # Should handle missing data gracefully
  expect_warning(result <- calculate_risk_scores(test_data, scores = "frs"))
  expect_equal(nrow(result), 3)
  expect_equal(rownames(result), rownames(test_data))
})

test_that("calculate_risk_scores preserves row names", {
  # Create minimal test data
  test_data <- data.frame(
    age = c(45, 55),
    gender = c("male", "female"),
    tc_mgdl = c(200, 220),
    hdl_mgdl = c(40, 50),
    sbp = c(130, 140),
    curr_smok = c(0, 1),
    cvhx_dm = c(0, 0),
    bp_med = c(0, 1)
  )
  rownames(test_data) <- c("Patient_A", "Patient_B")
  
  # Test that row names are preserved
  result <- calculate_risk_scores(test_data, scores = "frs", handle_missing = "na")
  expect_equal(rownames(result), rownames(test_data))
})

test_that("calculate_risk_scores handles ethnicity mappings", {
  # Test data without ethnicity columns
  test_data <- data.frame(
    age = c(45, 55),
    gender = c("male", "female"),
    tc_mgdl = c(200, 220),
    hdl_mgdl = c(40, 50),
    sbp = c(130, 140),
    curr_smok = c(0, 1),
    cvhx_dm = c(0, 0),
    bp_med = c(0, 1),
    ethnicity = c("european", "asian")
  )
  rownames(test_data) <- paste0("S", 1:2)
  
  # Should handle ethnicity mappings internally
  result <- calculate_risk_scores(test_data, scores = "ascvd", handle_missing = "na")
  expect_equal(nrow(result), 2)
  expect_equal(rownames(result), rownames(test_data))
})

test_that("calculate_risk_scores validates input", {
  # Test with non-data.frame input
  expect_error(calculate_risk_scores("not_a_dataframe"), "data must be a data frame")
  
  # Test with empty data frame
  empty_data <- data.frame()
  expect_error(calculate_risk_scores(empty_data), "replacement has.*row.*data has")
})