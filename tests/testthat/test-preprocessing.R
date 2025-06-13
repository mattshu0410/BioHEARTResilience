# Tests for data preprocessing functions

test_that("prepare_cohort_data works correctly", {
  # Create test data
  test_data <- data.frame(
    age = c(45, 55, 65),
    gender = c("male", "female", "male"),
    tc = c(5.0, 5.5, 4.8),  # mmol/L
    hdl = c(1.2, 1.4, 1.1),  # mmol/L
    sbp = c(120, 140, 130),
    curr_smok = c(0, 1, 0),
    cvhx_dm = c(0, 0, 1),
    bp_med = c(0, 1, 1),
    cacs = c(0, 15, 45)
  )
  rownames(test_data) <- paste0("S", 1:3)
  
  # Test basic preparation
  result <- prepare_cohort_data(test_data, cholesterol_unit = "mmol/L")
  
  expect_true(all(c("tc_mgdl", "hdl_mgdl") %in% names(result)))
  expect_equal(nrow(result), 3)
  expect_equal(rownames(result), rownames(test_data))
  
  # Check unit conversion
  expect_equal(result$tc_mgdl[1], 5.0 * 38.67)
  expect_equal(result$hdl_mgdl[1], 1.2 * 38.67)
  
  # Test with mg/dL input
  test_data_mgdl <- test_data
  test_data_mgdl$tc <- test_data_mgdl$tc * 38.67
  test_data_mgdl$hdl <- test_data_mgdl$hdl * 38.67
  
  result_mgdl <- prepare_cohort_data(test_data_mgdl, cholesterol_unit = "mg/dL")
  expect_equal(result_mgdl$tc_mgdl, test_data_mgdl$tc)
})

test_that("prepare_cohort_data handles missing columns", {
  # Missing required column
  incomplete_data <- data.frame(
    age = c(45, 55),
    gender = c("male", "female")
    # Missing other required columns
  )
  
  expect_error(prepare_cohort_data(incomplete_data), "Required columns missing")
})

test_that("recode_ethnicity works correctly", {
  ethnicity_values <- c("european", "asian", "african", "hispanic", "other")
  
  # Test ASCVD recoding
  result_ascvd <- recode_ethnicity(ethnicity_values, target_score = "ascvd")
  expected_ascvd <- c("white", "other", "aa", "other", "other")
  expect_equal(result_ascvd$ethnicity_ascvd, expected_ascvd)
  
  # Test MESA recoding
  result_mesa <- recode_ethnicity(ethnicity_values, target_score = "mesa")
  expected_mesa <- c("white", "chinese", "aa", "hispanic", "white")
  expect_equal(result_mesa$ethnicity_mesa, expected_mesa)
  
  # Test all recoding
  result_all <- recode_ethnicity(ethnicity_values, target_score = "all")
  expect_true(all(c("ethnicity_ascvd", "ethnicity_mesa") %in% names(result_all)))
})

test_that("validate_risk_data works correctly", {
  # Create test data with various age ranges
  test_data <- data.frame(
    age = c(25, 35, 45, 55, 65, 75, 85),  # Different ages for different scores
    gender = rep("male", 7),
    tc_mgdl = rep(200, 7),
    hdl_mgdl = rep(50, 7),
    sbp = rep(130, 7),
    curr_smok = rep(0, 7),
    cvhx_dm = rep(0, 7),
    bp_med = rep(0, 7)
  )
  
  # Test FRS validation (age 30-74)
  frs_valid <- validate_risk_data(test_data, scores = "frs")
  expect_equal(sum(frs_valid), 4)  # Ages 35, 45, 55, 65 (not 25, 75, 85)
  
  # Test ASCVD validation (age 20-79, cholesterol ranges)
  ascvd_valid <- validate_risk_data(test_data, scores = "ascvd")
  expect_equal(sum(ascvd_valid), 6)  # All except age 85
  
  # Test MESA validation (age 45-85)
  mesa_valid <- validate_risk_data(test_data, scores = "mesa")
  expect_equal(sum(mesa_valid), 5)  # Ages 45, 55, 65, 75, 85
})

test_that("prepare_cohort_data validates ranges", {
  # Test data with out-of-range values
  test_data <- data.frame(
    age = c(15, 45, 105),  # One too young, one too old
    gender = c("male", "female", "male"),
    tc = c(1.0, 5.0, 15.0),  # One too low, one too high
    hdl = c(0.2, 1.2, 5.0),  # One too low, one too high
    sbp = c(50, 120, 300),   # One too low, one too high
    curr_smok = c(0, 1, 0),
    cvhx_dm = c(0, 0, 1),
    bp_med = c(0, 1, 1),
    cacs = c(0, 15, 45)
  )
  
  # Should produce warnings for out-of-range values
  expect_warning(result <- prepare_cohort_data(test_data, validate = TRUE))
})

test_that("prepare_cohort_data preserves ID column", {
  # Test data with custom ID column
  test_data <- data.frame(
    BioHEART_ID = c("SUBJ_001", "SUBJ_002", "SUBJ_003"),
    age = c(45, 55, 65),
    gender = c("male", "female", "male"),
    tc = c(5.0, 5.5, 4.8),
    hdl = c(1.2, 1.4, 1.1),
    sbp = c(120, 140, 130),
    curr_smok = c(0, 1, 0),
    cvhx_dm = c(0, 0, 1),
    bp_med = c(0, 1, 1),
    cacs = c(0, 15, 45)
  )
  
  # Test with ID column specified
  result <- prepare_cohort_data(test_data, id_col = "BioHEART_ID", cholesterol_unit = "mmol/L")
  
  # Check that ID column is preserved and renamed to "id"
  expect_true("id" %in% names(result))
  expect_equal(result$id, test_data$BioHEART_ID)
  expect_equal(nrow(result), 3)
  
  # Test without ID column specified
  test_data_no_id <- test_data[, !names(test_data) %in% "BioHEART_ID"]
  result_no_id <- prepare_cohort_data(test_data_no_id, cholesterol_unit = "mmol/L")
  expect_false("id" %in% names(result_no_id))
  
  # Test with missing CACS (should remove rows but preserve remaining IDs)
  test_data_missing_cacs <- test_data
  test_data_missing_cacs$cacs[2] <- NA
  
  result_missing <- prepare_cohort_data(test_data_missing_cacs, id_col = "BioHEART_ID", cholesterol_unit = "mmol/L")
  expect_equal(nrow(result_missing), 2)  # One row removed
  expect_true("id" %in% names(result_missing))
  expect_equal(result_missing$id, c("SUBJ_001", "SUBJ_003"))  # Correct IDs preserved
})