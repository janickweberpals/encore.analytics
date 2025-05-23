% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smd_agreement.R
\name{smd_agreement}
\alias{smd_agreement}
\title{Calculate Standardized Mean Difference (SMD) Between RCT and RWE Results}
\usage{
smd_agreement(
  rct_estimate,
  rct_lower,
  rct_upper,
  rwe_estimate,
  rwe_lower,
  rwe_upper
)
}
\arguments{
\item{rct_estimate}{Numeric. Point estimate from the RCT (typically log hazard ratio or log odds ratio)}

\item{rct_lower}{Numeric. Lower bound of the confidence interval from RCT}

\item{rct_upper}{Numeric. Upper bound of the confidence interval from RCT}

\item{rwe_estimate}{Numeric. Point estimate from the RWE study}

\item{rwe_lower}{Numeric. Lower bound of the confidence interval from RWE}

\item{rwe_upper}{Numeric. Upper bound of the confidence interval from RWE}
}
\value{
Returns a numeric value representing the standardized mean difference.
Larger absolute values indicate greater disagreement between RCT and RWE results.
}
\description{
This function calculates the standardized mean difference (SMD) between results from
a randomized controlled trial (RCT) and its real-world evidence (RWE) emulation.
The SMD provides a measure of agreement between the two results, taking into account
both the point estimates and their corresponding uncertainty.
}
\details{
The SMD is calculated as the difference between RCT and RWE estimates, divided by
the square root of the sum of their variances. Variances are derived from the
confidence intervals assuming normal distribution (using 1.96 for 95% CI).

The formula used is:
SMD = (θ_RCT - θ_RWE) / sqrt(Var(θ_RCT) + Var(θ_RWE))
}
\examples{
# Example with hazard ratios (log scale)
smd <- smd_agreement(
  rct_estimate = log(0.87),
  rct_lower = log(0.78),
  rct_upper = log(0.97),
  rwe_estimate = log(0.82),
  rwe_lower = log(0.76),
  rwe_upper = log(0.87)
  )

}
\references{
For methodology on comparing RCT and RWE results using standardized differences.
}
