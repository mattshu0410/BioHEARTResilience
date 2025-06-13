% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ensemble_scores.R
\name{ensemble_risk_score}
\alias{ensemble_risk_score}
\title{Create ensemble risk score from multiple cardiovascular risk scores}
\usage{
ensemble_risk_score(risk_scores, score_cols = NULL, min_scores = 1)
}
\arguments{
\item{risk_scores}{Data frame with calculated risk scores (from calculate_risk_scores)}

\item{score_cols}{Character vector of score column names to include in ensemble.
If NULL, automatically detects columns ending in "_10y"}

\item{min_scores}{Minimum number of valid scores required per row (default: 1)}
}
\value{
Data frame with normalized scores and ensemble average (preserves row names)
}
\description{
Combines multiple risk scores using ordered quantile normalization and standardization,
following the methodology from the BioHEART resilience paper. For subjects with missing
scores, averages available scores with informative warnings.
}
\examples{
\dontrun{
# Create ensemble from all available scores
ensemble <- ensemble_risk_score(risk_scores)

# Create ensemble from specific scores only
ensemble <- ensemble_risk_score(risk_scores,
                               score_cols = c("frs_10y", "ascvd_10y"))
}
}
