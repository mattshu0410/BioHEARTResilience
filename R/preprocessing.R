#' Prepare cohort data for resilience analysis
#'
#' This function prepares and validates cohort data for use in resilience analysis.
#' It ensures all required columns are present, validates data types and ranges,
#' and converts units as needed.
#'
#' @param data Data frame containing cohort data
#' @param cacs_col Name of CACS column (default: "cacs")
#' @param age_col Name of age column (default: "age")
#' @param gender_col Name of gender column (default: "gender")
#' @param tc_col Name of total cholesterol column (default: "tc")
#' @param hdl_col Name of HDL cholesterol column (default: "hdl")
#' @param sbp_col Name of systolic BP column (default: "sbp")
#' @param smoking_col Name of smoking status column (default: "curr_smok")
#' @param diabetes_col Name of diabetes status column (default: "cvhx_dm")
#' @param bp_med_col Name of BP medication column (default: "bp_med")
#' @param lipid_med_col Name of lipid medication column (default: "lipid_med")
#' @param fh_ihd_col Name of family history column (default: "fh_ihd")
#' @param ethnicity_col Name of ethnicity column (default: "ethnicity")
#' @param cholesterol_unit Unit of cholesterol values ("mmol/L" or "mg/dL")
#' @param validate Logical, whether to validate data ranges
#' @return Prepared data frame with standardized column names and units
#' @export
#' @examples
#' \dontrun{
#' # Prepare data with cholesterol in mmol/L
#' prepared_data <- prepare_cohort_data(
#'   raw_data,
#'   cholesterol_unit = "mmol/L"
#' )
#' }
prepare_cohort_data <- function(data,
                              cacs_col = "cacs",
                              age_col = "age",
                              gender_col = "gender",
                              tc_col = "tc",
                              hdl_col = "hdl",
                              sbp_col = "sbp",
                              smoking_col = "curr_smok",
                              diabetes_col = "cvhx_dm",
                              bp_med_col = "bp_med",
                              lipid_med_col = "lipid_med",
                              fh_ihd_col = "fh_ihd",
                              ethnicity_col = "ethnicity",
                              cholesterol_unit = c("mmol/L", "mg/dL"),
                              validate = TRUE) {
  
  cholesterol_unit <- match.arg(cholesterol_unit)
  
  # Create column mapping
  col_mapping <- c(
    cacs = cacs_col,
    age = age_col,
    gender = gender_col,
    tc = tc_col,
    hdl = hdl_col,
    sbp = sbp_col,
    curr_smok = smoking_col,
    cvhx_dm = diabetes_col,
    bp_med = bp_med_col,
    lipid_med = lipid_med_col,
    fh_ihd = fh_ihd_col,
    ethnicity = ethnicity_col
  )
  
  # Check for required columns
  required_cols <- c(cacs_col, age_col, gender_col, tc_col, hdl_col, 
                    sbp_col, smoking_col, diabetes_col, bp_med_col)
  validate_columns(data, required_cols)
  
  # Create standardized data frame preserving row names
  result <- data.frame(row.names = rownames(data), stringsAsFactors = FALSE)
  
  # Map columns to standard names
  for (std_name in names(col_mapping)) {
    orig_col <- col_mapping[std_name]
    if (orig_col %in% names(data)) {
      result[[std_name]] <- data[[orig_col]]
    }
  }
  
  # Convert cholesterol units if needed
  if (cholesterol_unit == "mmol/L") {
    result$tc_mgdl <- convert_cholesterol_units(result$tc)
    result$hdl_mgdl <- convert_cholesterol_units(result$hdl)
  } else {
    result$tc_mgdl <- result$tc
    result$hdl_mgdl <- result$hdl
  }
  
  # Validate and standardize gender
  result$gender <- validate_gender(result$gender)
  
  # Validate binary variables
  if (validate) {
    validate_binary(result$curr_smok, "Smoking status")
    validate_binary(result$cvhx_dm, "Diabetes status")
    validate_binary(result$bp_med, "BP medication")
    
    if ("lipid_med" %in% names(result)) {
      validate_binary(result$lipid_med, "Lipid medication")
    }
    
    if ("fh_ihd" %in% names(result)) {
      validate_binary(result$fh_ihd, "Family history")
    }
    
    # Validate ranges
    check_range(result$age, 18, 100, "Age")
    check_range(result$sbp, 70, 250, "Systolic BP")
    check_range(result$tc_mgdl, 100, 500, "Total cholesterol")
    check_range(result$hdl_mgdl, 10, 150, "HDL cholesterol")
    check_range(result$cacs, 0, Inf, "CACS")
  }
  
  # Check for missing values
  check_missing_values(result, verbose = TRUE)
  
  # Add attributes for tracking
  attr(result, "cholesterol_unit_original") <- cholesterol_unit
  attr(result, "preparation_date") <- Sys.Date()
  
  return(result)
}

#' Recode ethnicity for risk score calculations
#'
#' Maps ethnicity values to categories required by different risk scores
#'
#' @param ethnicity Vector of ethnicity values
#' @param target_score Target risk score ("ascvd", "mesa", or "all")
#' @param mapping_table Optional custom mapping table
#' @return Data frame with recoded ethnicity columns for each score
#' @export
#' @examples
#' \dontrun{
#' ethnicity_coded <- recode_ethnicity(
#'   data$ethnicity,
#'   target_score = "all"
#' )
#' }
recode_ethnicity <- function(ethnicity, 
                           target_score = c("ascvd", "mesa", "all"),
                           mapping_table = NULL) {
  
  target_score <- match.arg(target_score)
  
  # Default mappings based on paper methodology
  if (is.null(mapping_table)) {
    mapping_table <- data.frame(
      original = c("european", "indigenous_australia", "polynesian", 
                  "african", "asian", "indian", "middle_eastern", 
                  "hispanic", "other", "mixed"),
      ascvd = c("white", "other", "other", 
               "aa", "other", "other", "other", 
               "other", "other", "other"),
      mesa = c("white", "aa", "white", 
              "aa", "chinese", "other", "white", 
              "hispanic", "white", "white"),
      stringsAsFactors = FALSE
    )
  }
  
  # Standardize ethnicity input
  ethnicity_lower <- tolower(as.character(ethnicity))
  
  # Create result data frame
  result <- data.frame(
    ethnicity_original = ethnicity,
    stringsAsFactors = FALSE
  )
  
  # Map to ASCVD categories
  if (target_score %in% c("ascvd", "all")) {
    result$ethnicity_ascvd <- "other"  # Default
    for (i in seq_len(nrow(mapping_table))) {
      mask <- ethnicity_lower == mapping_table$original[i]
      result$ethnicity_ascvd[mask] <- mapping_table$ascvd[i]
    }
  }
  
  # Map to MESA categories
  if (target_score %in% c("mesa", "all")) {
    result$ethnicity_mesa <- "white"  # Default
    for (i in seq_len(nrow(mapping_table))) {
      mask <- ethnicity_lower == mapping_table$original[i]
      result$ethnicity_mesa[mask] <- mapping_table$mesa[i]
    }
  }
  
  return(result)
}

#' Validate data for risk score calculations
#'
#' Checks that data meets requirements for specified risk scores
#'
#' @param data Prepared data frame
#' @param scores Character vector of scores to validate for
#' @return Logical vector indicating which rows are valid for all scores
#' @export
validate_risk_data <- function(data, scores = c("frs", "ascvd", "mesa", "score2")) {
  
  n_rows <- nrow(data)
  valid <- rep(TRUE, n_rows)
  
  # FRS requirements: age 30-74
  if ("frs" %in% scores) {
    frs_valid <- data$age >= 30 & data$age <= 74
    if (sum(!frs_valid, na.rm = TRUE) > 0) {
      message(sprintf("FRS: %d rows outside age range 30-74", 
                     sum(!frs_valid, na.rm = TRUE)))
    }
    valid <- valid & frs_valid
  }
  
  # ASCVD requirements
  if ("ascvd" %in% scores) {
    ascvd_valid <- (data$age >= 20 & data$age <= 79 &
                   data$tc_mgdl >= 130 & data$tc_mgdl <= 320 &
                   data$hdl_mgdl >= 20 & data$hdl_mgdl <= 100 &
                   data$sbp >= 90 & data$sbp <= 200)
    
    if (sum(!ascvd_valid, na.rm = TRUE) > 0) {
      message(sprintf("ASCVD: %d rows outside valid ranges", 
                     sum(!ascvd_valid, na.rm = TRUE)))
    }
    valid <- valid & ascvd_valid
  }
  
  # MESA requirements: age 45-85
  if ("mesa" %in% scores) {
    mesa_valid <- data$age >= 45 & data$age <= 85
    if (sum(!mesa_valid, na.rm = TRUE) > 0) {
      message(sprintf("MESA: %d rows outside age range 45-85", 
                     sum(!mesa_valid, na.rm = TRUE)))
    }
    valid <- valid & mesa_valid
  }
  
  # SCORE2 has no strict requirements but works best for certain ages
  if ("score2" %in% scores) {
    score2_info <- data$age < 40 | data$age > 75
    if (sum(score2_info, na.rm = TRUE) > 0) {
      message(sprintf("SCORE2: %d rows outside optimal age range 40-75", 
                     sum(score2_info, na.rm = TRUE)))
    }
  }
  
  # Check for missing required values
  required_vars <- c("age", "gender", "tc_mgdl", "hdl_mgdl", 
                    "sbp", "curr_smok", "cvhx_dm", "bp_med")
  
  for (var in required_vars) {
    if (var %in% names(data)) {
      valid <- valid & !is.na(data[[var]])
    }
  }
  
  message(sprintf("Total valid rows for all scores: %d of %d (%.1f%%)",
                 sum(valid), n_rows, 100 * sum(valid) / n_rows))
  
  return(valid)
}