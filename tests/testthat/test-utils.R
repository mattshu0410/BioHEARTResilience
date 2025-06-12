# Tests for utility functions

test_that("convert_cholesterol_units works correctly", {
  # Test basic conversion
  expect_equal(convert_cholesterol_units(5.2), 5.2 * 38.67)
  expect_equal(convert_cholesterol_units(1.3), 1.3 * 38.67)
  
  # Test vector input
  input_vec <- c(4.0, 5.5, 1.2)
  expected <- input_vec * 38.67
  expect_equal(convert_cholesterol_units(input_vec), expected)
  
  # Test with NA values
  input_with_na <- c(4.0, NA, 5.5)
  result <- convert_cholesterol_units(input_with_na)
  expect_true(is.na(result[2]))
  expect_equal(result[c(1, 3)], c(4.0, 5.5) * 38.67)
})

test_that("standardize works correctly", {
  # Test basic standardization
  x <- c(1, 2, 3, 4, 5)
  result <- standardize(x)
  expect_equal(mean(result), 0, tolerance = 1e-10)
  expect_equal(sd(result), 1, tolerance = 1e-10)
  
  # Test with constant values
  constant_x <- c(5, 5, 5, 5)
  expect_warning(result_constant <- standardize(constant_x))
  expect_equal(result_constant, c(0, 0, 0, 0))
  
  # Test with NA values
  x_with_na <- c(1, 2, NA, 4, 5)
  result_na <- standardize(x_with_na, na.rm = TRUE)
  expect_false(any(is.na(result_na[c(1, 2, 4, 5)])))
  expect_true(is.na(result_na[3]))
})

test_that("validate_columns works correctly", {
  test_data <- data.frame(a = 1:5, b = 6:10, c = 11:15)
  
  # Should pass with existing columns
  expect_silent(validate_columns(test_data, c("a", "b")))
  
  # Should error with missing columns
  expect_error(validate_columns(test_data, c("a", "d")), "Required columns missing")
})

test_that("validate_binary works correctly", {
  # Valid binary data
  valid_binary <- c(0, 1, 0, 1, 1)
  expect_silent(validate_binary(valid_binary, "test_var"))
  
  # Invalid binary data
  invalid_binary <- c(0, 1, 2, 1)
  expect_error(validate_binary(invalid_binary, "test_var"), "must be binary")
  
  # With NA values
  binary_with_na <- c(0, 1, NA, 1)
  expect_silent(validate_binary(binary_with_na, "test_var"))
})

test_that("validate_gender works correctly", {
  # Test various gender formats
  expect_equal(validate_gender(c("male", "female")), c("male", "female"))
  expect_equal(validate_gender(c("M", "F")), c("male", "female"))
  expect_equal(validate_gender(c("1", "2")), c("male", "female"))
  expect_equal(validate_gender(c("MALE", "FEMALE")), c("male", "female"))
  
  # Test invalid gender
  expect_error(validate_gender(c("male", "other")), "Invalid gender values")
  
  # Test with NA
  result_with_na <- validate_gender(c("male", NA, "female"))
  expect_equal(result_with_na, c("male", NA, "female"))
})

test_that("check_range works correctly", {
  # Test within range
  x_valid <- c(25, 30, 35)
  expect_warning(result <- check_range(x_valid, 20, 40, "test"), NA)
  expect_equal(result, c(TRUE, TRUE, TRUE))
  
  # Test outside range
  x_invalid <- c(15, 30, 45)
  expect_warning(result <- check_range(x_invalid, 20, 40, "test"))
  expect_equal(result, c(FALSE, TRUE, FALSE))
})

test_that("check_missing_values works correctly", {
  # Data with no missing values
  data_complete <- data.frame(a = 1:5, b = 6:10)
  result_complete <- check_missing_values(data_complete, verbose = FALSE)
  expect_equal(nrow(result_complete), 0)
  
  # Data with missing values
  data_missing <- data.frame(a = c(1, NA, 3), b = c(4, 5, NA))
  result_missing <- check_missing_values(data_missing, verbose = FALSE)
  expect_equal(nrow(result_missing), 2)
  expect_equal(result_missing$n_missing, c(1, 1))
})