#' Example BioHEART-style cohort data
#'
#' A simulated dataset containing clinical, demographic, and CACS data
#' for demonstrating the BioHEART resilience analysis methodology.
#'
#' @format A data frame with 500 rows and 16 variables:
#' \describe{
#'   \item{age}{Age in years (30-80)}
#'   \item{gender}{Gender ("male" or "female")}
#'   \item{ethnicity}{Ethnicity category}
#'   \item{tc}{Total cholesterol in mmol/L}
#'   \item{hdl}{HDL cholesterol in mmol/L}
#'   \item{sbp}{Systolic blood pressure in mmHg}
#'   \item{curr_smok}{Current smoking status (0/1)}
#'   \item{cvhx_dm}{Diabetes mellitus status (0/1)}
#'   \item{bp_med}{Blood pressure medication use (0/1)}
#'   \item{lipid_med}{Lipid medication use (0/1)}
#'   \item{fh_ihd}{Family history of ischemic heart disease (0/1)}
#'   \item{cacs}{Coronary artery calcium score (Agatston units)}
#'   \item{bmi}{Body mass index}
#'   \item{ace_arb}{ACE inhibitor or ARB use (0/1)}
#'   \item{statin}{Statin use (0/1)}
#'   \item{subject_id}{Subject identifier}
#' }
#'
#' @details
#' This simulated dataset mimics the structure and distributions found in
#' real cardiovascular cohort studies. The data includes:
#'
#' \itemize{
#'   \item Realistic age and gender distributions
#'   \item Cardiovascular risk factors with appropriate correlations
#'   \item CACS values that follow zero-inflated distributions
#'   \item Missing data patterns similar to real clinical studies
#'   \item Medication use patterns reflecting clinical practice
#' }
#'
#' The dataset is designed to demonstrate all features of the resilience
#' analysis pipeline including handling of missing data, multiple risk score
#' calculations, and classification of resilience phenotypes.
#'
#' @source Simulated data based on BioHEART cohort characteristics
"example_cohort"

#' Framingham Risk Score Coefficients
#'
#' Coefficient data for calculating Framingham Risk Score from the CVrisk package.
#' These coefficients are used internally by CVrisk functions for risk calculation.
#'
#' @format A data structure containing coefficients for FRS calculation
#' @source CVrisk package
"frs_coef"

#' ACC/AHA ASCVD Pooled Cohort Equations Coefficients
#'
#' Coefficient data for calculating ACC/AHA ASCVD risk from the CVrisk package.
#' These coefficients are used internally by CVrisk functions for risk calculation.
#'
#' @format A data structure containing coefficients for ASCVD calculation
#' @source CVrisk package
"ascvd_pooled_coef"

#' MESA CHD Risk Score Coefficients
#'
#' Coefficient data for calculating MESA CHD risk from the CVrisk package.
#' These coefficients are used internally by CVrisk functions for risk calculation.
#'
#' @format A data structure containing coefficients for MESA calculation
#' @source CVrisk package
"mesa_coef"