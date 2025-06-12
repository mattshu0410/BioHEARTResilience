# Tests for ensemble scoring functions

test_that("normalize_risk_score works correctly", {
  # Test basic normalization
  test_scores <- c(5, 10, 15, 20, 25)
  result <- normalize_risk_score(test_scores)
  
  expect_equal(length(result), length(test_scores))
  expect_true(all(is.finite(result)))
  
  # Test with return_transform = TRUE
  result_with_transform <- normalize_risk_score(test_scores, return_transform = TRUE)
  expect_true(is.list(result_with_transform))
  expect_true(all(c("values", "transform", "original_stats") %in% names(result_with_transform)))
  
  # Test with all missing values
  all_na <- rep(NA_real_, 5)  # Ensure numeric NA
  expect_warning(result_na <- normalize_risk_score(all_na))
  expect_true(all(is.na(result_na)))
  
  # Test with non-numeric input
  expect_error(normalize_risk_score(c("a", "b", "c")), "Input must be numeric")
})

test_that("ensemble_risk_score works correctly", {
  # Create test risk score data
  test_risk_scores <- data.frame(
    frs_10y = c(5, 10, 15, 8, 12),
    ascvd_10y = c(4, 12, 18, 7, 10),
    mesa_10y = c(6, 9, 16, 9, 11),
    score2_10y = c(3, 8, 14, 6, 9)
  )
  rownames(test_risk_scores) <- paste0("S", 1:5)
  
  # Test basic ensemble creation
  result <- ensemble_risk_score(test_risk_scores)
  
  expect_equal(nrow(result), 5)
  expect_equal(rownames(result), rownames(test_risk_scores))
  expect_true("average_norm_score" %in% names(result))
  expect_true("n_scores_used" %in% names(result))
  
  # Check that normalized columns are created
  norm_cols <- grep("^ordernorm_", names(result), value = TRUE)
  expect_equal(length(norm_cols), 4)
  
  # Test with specific score columns
  result_subset <- ensemble_risk_score(test_risk_scores, 
                                     score_cols = c("frs_10y", "ascvd_10y"))
  norm_cols_subset <- grep("^ordernorm_", names(result_subset), value = TRUE)
  expect_equal(length(norm_cols_subset), 2)
})

test_that("ensemble_risk_score handles missing data", {
  # Create test data with missing values
  test_risk_scores <- data.frame(
    frs_10y = c(5, NA, 15, 8, 12),
    ascvd_10y = c(4, 12, NA, 7, 10),
    mesa_10y = c(6, 9, 16, NA, 11),
    score2_10y = c(3, 8, 14, 6, NA)
  )
  rownames(test_risk_scores) <- paste0("S", 1:5)
  
  result <- ensemble_risk_score(test_risk_scores, min_scores = 1)
  
  # Should still produce results for all rows
  expect_equal(nrow(result), 5)
  expect_true(all(!is.na(result$average_norm_score)))  # With min_scores = 1, all should have ensemble scores
  
  # Check n_scores_used
  expect_equal(result$n_scores_used[1], 4)  # First row has all scores
  expect_equal(result$n_scores_used[2], 3)  # Second row missing FRS
})

test_that("ensemble_risk_score validates input", {
  # Test with non-data.frame
  expect_error(ensemble_risk_score("not_data_frame"), "risk_scores must be a data frame")
  
  # Test with missing score columns
  bad_data <- data.frame(a = 1:3, b = 4:6)
  expect_error(ensemble_risk_score(bad_data, score_cols = c("missing_col")), 
               "Score columns not found")
  
  # Test with no valid score columns
  no_scores_data <- data.frame(other_col = 1:3)
  expect_error(ensemble_risk_score(no_scores_data), "No valid score columns found")
})

test_that("summarize_ensemble_scores works correctly", {
  # Create test ensemble results
  test_ensemble <- data.frame(
    ordernorm_frs = rnorm(10),
    ordernorm_ascvd = rnorm(10),
    average_norm_score = rnorm(10),
    n_scores_used = sample(2:4, 10, replace = TRUE)
  )
  rownames(test_ensemble) <- paste0("S", 1:10)
  
  result <- summarize_ensemble_scores(test_ensemble)
  
  expect_true(inherits(result, "ensemble_summary"))
  expect_true(all(c("normalized_summary", "ensemble_summary", "score_usage") %in% names(result)))
  
  # Test print method
  expect_output(print(result), "Ensemble Risk Score Summary")
})

test_that("ensemble_risk_score min_scores parameter works", {
  # Create test data where some subjects have few scores
  test_risk_scores <- data.frame(
    frs_10y = c(5, NA, NA, 8),     # Subject 2,3 missing
    ascvd_10y = c(4, 12, NA, 7),   # Subject 3 missing
    mesa_10y = c(6, NA, NA, 9),    # Subject 2,3 missing
    score2_10y = c(3, NA, 14, 6)   # Subject 2 missing
  )
  rownames(test_risk_scores) <- paste0("S", 1:4)
  
  # With min_scores = 3, subjects 2 and 3 should have NA ensemble scores
  result_strict <- ensemble_risk_score(test_risk_scores, min_scores = 3)
  expect_true(is.na(result_strict$average_norm_score[2]))  # Subject 2: only 1 score
  expect_true(is.na(result_strict$average_norm_score[3]))  # Subject 3: only 1 score
  expect_false(is.na(result_strict$average_norm_score[1])) # Subject 1: 4 scores
  expect_false(is.na(result_strict$average_norm_score[4])) # Subject 4: 3 scores
  
  # With min_scores = 1, all should have ensemble scores
  result_lenient <- ensemble_risk_score(test_risk_scores, min_scores = 1)
  expect_true(all(!is.na(result_lenient$average_norm_score)))
})