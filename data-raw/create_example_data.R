# Create example cohort data for BioHEART Resilience package
# This script generates realistic dummy data for demonstration purposes

library(MASS)  # For mvrnorm

set.seed(12345)  # For reproducibility

n <- 500  # Number of subjects

# Generate independent variables first, then add correlations
age_base <- rnorm(n, 55, 12)
tc_base <- rnorm(n, 5.2, 0.8)
sbp_base <- rnorm(n, 130, 20)
hdl_base <- rnorm(n, 1.3, 0.3)

# Add some correlations manually
age <- pmax(30, pmin(80, age_base))
tc <- pmax(2.5, pmin(10, tc_base + 0.2 * scale(age)[,1]))  # TC increases with age
sbp <- pmax(90, pmin(200, sbp_base + 0.3 * scale(age)[,1]))  # SBP increases with age
hdl <- pmax(0.5, pmin(3.0, hdl_base - 0.1 * scale(age)[,1]))  # HDL decreases with age

mvn_data <- cbind(age, tc, sbp, hdl)

# Create base data frame
example_cohort <- data.frame(
  subject_id = paste0("S", sprintf("%03d", 1:n)),
  
  # Demographics
  age = round(pmax(30, pmin(80, mvn_data[, 1]))),
  gender = sample(c("male", "female"), n, replace = TRUE, prob = c(0.55, 0.45)),
  ethnicity = sample(c("european", "asian", "african", "hispanic", "other"), 
                    n, replace = TRUE, prob = c(0.4, 0.25, 0.15, 0.15, 0.05)),
  
  # Lab values (in mmol/L)
  tc = pmax(2.5, pmin(10, mvn_data[, 2])),  # Total cholesterol
  hdl = pmax(0.5, pmin(3.0, mvn_data[, 4])), # HDL cholesterol
  
  # Clinical measures
  sbp = round(pmax(90, pmin(200, mvn_data[, 3]))),  # Systolic BP
  bmi = round(rnorm(n, 27, 5), 1),
  
  stringsAsFactors = FALSE
)

# Adjust BMI to be realistic
example_cohort$bmi <- pmax(18, pmin(45, example_cohort$bmi))

# Generate binary risk factors with age/gender dependencies
example_cohort$curr_smok <- rbinom(n, 1, pmax(0.05, 0.35 - (example_cohort$age - 30) * 0.005))
example_cohort$cvhx_dm <- rbinom(n, 1, pmax(0.05, (example_cohort$age - 30) * 0.008 + 
                                            ifelse(example_cohort$bmi > 30, 0.15, 0)))

# Generate family history
example_cohort$fh_ihd <- rbinom(n, 1, 0.25)

# Generate medication use (should correlate with risk factors)
bp_risk <- (example_cohort$sbp - 120) / 40 + (example_cohort$age - 50) / 30 + example_cohort$cvhx_dm * 0.3
example_cohort$ace_arb <- rbinom(n, 1, pmax(0.05, pmin(0.8, bp_risk * 0.3)))

lipid_risk <- (example_cohort$tc - 4) / 2 + (example_cohort$age - 50) / 25 + example_cohort$cvhx_dm * 0.2
example_cohort$statin <- rbinom(n, 1, pmax(0.05, pmin(0.7, lipid_risk * 0.25)))

# Create composite medication variables
example_cohort$bp_med <- pmax(example_cohort$ace_arb, 
                             rbinom(n, 1, bp_risk * 0.2))  # Other BP meds

example_cohort$lipid_med <- pmax(example_cohort$statin,
                                rbinom(n, 1, lipid_risk * 0.1))  # Other lipid meds

# Generate CACS with zero-inflation and realistic distribution
# Risk score approximation for CACS generation
risk_score <- scale(example_cohort$age * 0.3 + 
                   example_cohort$tc * 2 + 
                   example_cohort$sbp * 0.1 +
                   (example_cohort$gender == "male") * 3 +
                   example_cohort$curr_smok * 4 +
                   example_cohort$cvhx_dm * 5 -
                   example_cohort$hdl * 3)[, 1]

# Zero-inflated CACS generation
zero_prob <- 1 / (1 + exp(risk_score + 0.5))  # Probability of structural zero
structural_zeros <- rbinom(n, 1, zero_prob)

# For non-structural zeros, generate from negative binomial
cacs_nonzero <- rnbinom(n, size = 0.5, mu = exp(pmax(-2, pmin(5, risk_score + 1))))

example_cohort$cacs <- ifelse(structural_zeros == 1, 0, cacs_nonzero)

# Add some realistic missingness patterns
missing_indices <- list(
  tc = sample(n, 0.02 * n),      # 2% missing cholesterol
  hdl = sample(n, 0.02 * n),     # 2% missing HDL
  sbp = sample(n, 0.01 * n),     # 1% missing SBP
  fh_ihd = sample(n, 0.05 * n),  # 5% missing family history
  lipid_med = sample(n, 0.03 * n) # 3% missing lipid medication
)

for (var in names(missing_indices)) {
  example_cohort[missing_indices[[var]], var] <- NA
}

# Reorder columns
example_cohort <- example_cohort[, c("subject_id", "age", "gender", "ethnicity", 
                                   "tc", "hdl", "sbp", "bmi", "curr_smok", "cvhx_dm", 
                                   "bp_med", "lipid_med", "ace_arb", "statin", 
                                   "fh_ihd", "cacs")]

# Round numeric values appropriately
example_cohort$tc <- round(example_cohort$tc, 1)
example_cohort$hdl <- round(example_cohort$hdl, 1)

# Set row names to subject IDs for demonstration
rownames(example_cohort) <- example_cohort$subject_id

# Summary of the created data
cat("Example cohort data created:\n")
cat("N =", nrow(example_cohort), "subjects\n")
cat("Age range:", range(example_cohort$age), "\n")
cat("Gender distribution:", table(example_cohort$gender), "\n")
cat("CACS distribution:\n")
cat("  Zero CACS:", sum(example_cohort$cacs == 0), "/", nrow(example_cohort), 
    sprintf("(%.1f%%)\n", 100 * sum(example_cohort$cacs == 0) / nrow(example_cohort)))
cat("  Median CACS (non-zero):", median(example_cohort$cacs[example_cohort$cacs > 0]), "\n")
cat("  Max CACS:", max(example_cohort$cacs), "\n")

# Save the data
usethis::use_data(example_cohort, overwrite = TRUE)

cat("Data saved to data/example_cohort.rda\n")