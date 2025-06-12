# BioHEART Resilience Package

[\![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

An R package implementing the BioHEART methodology for cardiovascular disease resilience phenotype classification using coronary artery calcium scores (CACS) and ensemble risk scoring.

## Overview

This package implements the novel methodology described in the BioHEART research for classifying cardiovascular disease resilience phenotypes. It combines multiple cardiovascular risk scores using ordered quantile normalization, fits zero-inflated negative binomial models for CACS prediction, and calculates risk-adjusted CACS percentiles to classify individuals into resilience categories.

## Key Features

- **Multi-Risk Score Integration**: Supports FRS, ASCVD, MESA, and SCORE2 risk calculators
- **Ensemble Scoring**: Uses ordered quantile normalization for risk score combination
- **Zero-Inflated Modeling**: Fits ZINB models for CACS prediction accounting for zero-inflation
- **Resilience Classification**: Classifies individuals as resilient, reference, or susceptible
- **Comprehensive Validation**: Extensive input validation and missing data handling
- **Visualization Tools**: Built-in plotting functions for results interpretation

## Installation

### Development Version

```r
# Install development version from GitHub
if (\!require(devtools)) install.packages("devtools")
devtools::install_github("matthewshu/BioHEARTResilience")
```

### Dependencies

The package requires the following dependencies:
- `stats` (base R)
- `pscl` (>= 1.5.5) - for zero-inflated models
- `bestNormalize` (>= 1.8.0) - for ordered quantile normalization
- `CVrisk` (>= 1.1.0) - for cardiovascular risk scores
- `RiskScorescvd` (>= 0.2.0) - for additional risk scores

## Quick Start

```r
library(BioHEARTResilience)

# Load example data
data("example_cohort")

# Run complete resilience analysis
results <- resilience_analysis(
  data = example_cohort,
  cholesterol_unit = "mg/dL",
  risk_scores = c("frs", "ascvd", "mesa", "score2"),
  min_scores = 2,
  include_plots = TRUE
)

# View resilience classifications
head(results$classifications)

# Summary of results
print(results$risk_summary)
print(results$ensemble_summary)
```

## Data Requirements

Your dataset should include:

### Required Columns
- `age`: Age in years
- `gender`: "male"/"female" (or "M"/"F", "1"/"2")
- `tc` or `tc_mgdl`: Total cholesterol
- `hdl` or `hdl_mgdl`: HDL cholesterol
- `sbp`: Systolic blood pressure (mmHg)
- `curr_smok`: Current smoking status (0/1)
- `cvhx_dm`: Diabetes history (0/1)
- `bp_med`: Blood pressure medication (0/1)
- `lipid_med`: Lipid medication (0/1)
- `ethnicity`: Ethnicity for risk score calculation
- `cacs`: Coronary artery calcium score

## Testing

The package includes a comprehensive testing suite with 129 tests achieving 100% pass rate:

```r
# Run all tests
devtools::test()
```

## License

This project is licensed under the MIT License.

## Contributing

Contributions are welcome\! Please feel free to submit a Pull Request.

## Development Status

This package is actively maintained and developed. Current version includes:
- ✅ Complete risk score integration
- ✅ Zero-inflated CACS modeling  
- ✅ Resilience classification
- ✅ Comprehensive testing suite (129 tests, 100% pass rate)
- ✅ Extensive documentation and examples
- ✅ Visualization tools
