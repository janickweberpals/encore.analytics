#' Calculate Standardized Mean Difference (SMD) Between RCT and RWE Results
#'
#' @description
#' This function calculates the standardized mean difference (SMD) between results from
#' a randomized controlled trial (RCT) and its real-world evidence (RWE) emulation.
#' The SMD provides a measure of agreement between the two results, taking into account
#' both the point estimates and their corresponding uncertainty.
#'
#' @details
#' The SMD is calculated as the difference between RCT and RWE estimates, divided by
#' the square root of the sum of their variances. Variances are derived from the
#' confidence intervals assuming normal distribution (using 1.96 for 95% CI).
#'
#' The formula used is:
#' SMD = (θ_RCT - θ_RWE) / sqrt(Var(θ_RCT) + Var(θ_RWE))
#'
#' @param rct_estimate Numeric. Point estimate from the RCT (typically log hazard ratio or log odds ratio)
#' @param rct_lower Numeric. Lower bound of the confidence interval from RCT
#' @param rct_upper Numeric. Upper bound of the confidence interval from RCT
#' @param rwe_estimate Numeric. Point estimate from the RWE study
#' @param rwe_lower Numeric. Lower bound of the confidence interval from RWE
#' @param rwe_upper Numeric. Upper bound of the confidence interval from RWE
#'
#' @return
#' Returns a numeric value representing the standardized mean difference.
#' Larger absolute values indicate greater disagreement between RCT and RWE results.
#'
#' @examples
#' # Example with hazard ratios (log scale)
#' smd <- smd_agreement(
#'   rct_estimate = log(0.87),
#'   rct_lower = log(0.78),
#'   rct_upper = log(0.97),
#'   rwe_estimate = log(0.82),
#'   rwe_lower = log(0.76),
#'   rwe_upper = log(0.87)
#'   )
#'
#' @references
#' For methodology on comparing RCT and RWE results using standardized differences.
#'
#' @export
smd_agreement <- function(rct_estimate = NULL,
                          rct_lower = NULL,
                          rct_upper = NULL,
                          rwe_estimate = NULL,
                          rwe_lower = NULL,
                          rwe_upper = NULL
                          ){

  # Input validation
  required_args <- c("rct_estimate", "rct_lower", "rct_upper",
                    "rwe_estimate", "rwe_lower", "rwe_upper")

  # Check for missing values
  missing_args <- required_args[sapply(list(rct_estimate, rct_lower, rct_upper,
                                          rwe_estimate, rwe_lower, rwe_upper),
                                     is.null)]
  if (length(missing_args) > 0) {
    stop(sprintf("Missing required arguments: %s",
                paste(missing_args, collapse = ", ")))
  }

  # Check numeric inputs
  if (!all(sapply(list(rct_estimate, rct_lower, rct_upper,
                      rwe_estimate, rwe_lower, rwe_upper), is.numeric))) {
    stop("All inputs must be numeric values")
  }

  # Check bounds
  if (rct_lower >= rct_upper) {
    stop("RCT lower bound must be less than upper bound")
  }
  if (rwe_lower >= rwe_upper) {
    stop("RWE lower bound must be less than upper bound")
  }

  # Check if estimate is within bounds
  if (rct_estimate < rct_lower || rct_estimate > rct_upper) {
    warning("RCT estimate is outside its confidence interval bounds")
  }
  if (rwe_estimate < rwe_lower || rwe_estimate > rwe_upper) {
    warning("RWE estimate is outside its confidence interval bounds")
  }

  # numerator
  numerator <- (rct_estimate - rwe_estimate)

  # variances
  var_rct <- (rct_upper - rct_lower)/(2*1.96)
  var_rwe <- (rwe_upper - rwe_lower)/(2*1.96)

  # denominator
  denominator <- sqrt(var_rct^2 + var_rwe^2)

  # Final check for valid calculation
  if (is.infinite(denominator) || is.nan(denominator)) {
    stop("Invalid calculation: denominator is infinite or NaN")
  }

  # smd
  smd <- numerator/denominator

  return(smd)

}
