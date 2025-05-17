#' Manually fit and pool Cox proportional hazards model results from multiple imputed datasets
#'
#' @description
#' This function manually fits and pools results from Cox proportional hazards models using
#' a list of imputed datasets. It leverages the `mice::as.mira` and `mice::pool` functions to
#' ensure proper pooling of results across multiple imputations using Rubin's rules.
#'
#' @details
#' The function requires a list of imputed data frames with weights and optional cluster (matching) information,
#' as well as a formula for the Cox proportional hazards model. The data frames must include a column named
#' \code{weights}, and optionally a column named \code{subclass} (for matched datasets to indicate cluster membership).
#'
#' This function is particularly useful when working with imputed datasets that are not in the form of
#' \code{mimids} or \code{wimids} objects, such as when intermediate steps like raking weights
#' (via \code{raking_weights}) are applied. It provides a flexible way to fit and pool Cox models
#' while ensuring compatibility with Rubin's rules for multiple imputation.
#'
#' The function follows these steps:
#' 1. Fit a Cox proportional hazards model to each imputed dataset. If a \code{subclass} column is present,
#'    it is used as a cluster variable for matched pairs.
#' 2. Convert the list of fitted models into a \code{mira} object using \code{mice::as.mira}.
#' 3. Pool the results using \code{mice::pool}, which applies Rubin's rules for combining estimates
#'    and variances across imputations.
#' 4. Format the pooled results, including exponentiating the hazard ratios and calculating confidence intervals.
#'
#' @seealso
#' \code{\link[survival]{coxph}}, \code{\link[mice]{pool}}, \code{\link[mice]{as.mira}},
#' \code{\link[MatchThem]{matchthem}}, \code{\link[MatchThem]{weightthem}}
#'
#' @param x A list of imputed datasets with weights or raking weights (e.g., from \code{\link[encore.analytics]{raking_weights}})
#'          and optional cluster information (for matched datasets).
#' @param surv_formula A formula for the Cox proportional hazards model (default is \code{Surv(fu_itt_months, death_itt) ~ treat}).
#'
#' @return A data frame containing the pooled results, including hazard ratios, confidence intervals, and p-values.
#'
#' @md
#'
#' @export
#'
#' @examples
#' library(encore.analytics)
#' library(mice)
#' library(MatchThem)
#'
#' # Simulate a cohort with 500 patients and 20% missing data
#' data <- simulate_data(
#'   n = 500,
#'   imposeNA = TRUE,
#'   propNA = 0.2
#' )
#'
#' # Impute the data
#' set.seed(42)
#' mids <- mice(data, m = 5, print = FALSE)
#'
#' # Fit a propensity score model
#' fit <- as.formula(treat ~ dem_age_index_cont + dem_sex_cont + c_smoking_history)
#'
#' # Weight patients within each imputed dataset
#' wimids <- weightthem(
#'   formula = fit,
#'   datasets = mids,
#'   approach = "within",
#'   method = "glm",
#'   estimand = "ATO"
#' )
#'
#' # Create a list of imputed and weighted datasets
#' wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
#'
#' # Define a survival model formula
#' cox_fit <- as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat)
#'
#' # Fit and pool Cox proportional hazards model results
#' cox_pooling(wimids_list, surv_formula = cox_fit)
cox_pooling <- function(x,
                        surv_formula = stats::as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat)
                        ){

  # input checks----

  # check if object is a list
  assertthat::assert_that(inherits(x, "list") | inherits(x, "mild"), msg = "<cox_results_list> needs to be a list or mild object")
  # check if surv_formula is a formula
  assertthat::assert_that(inherits(surv_formula, "formula"), msg = "<surv_formula> needs to be a formula")
  # check if weights_col is in the data
  df_colnames <- colnames(x[[1]])
  assertthat::assert_that("weights" %in% df_colnames, msg = "<weights> not in <x>")

  # Step 1: fit Cox model to each imputed dataset----
  cox_fit <- function(i){

    # if cluster_col is in the data (matched datasets)
    if("subclass" %in% df_colnames){

      cox <- survival::coxph(
        data = x[[i]],
        formula = surv_formula,
        weights = weights,
        cluster = subclass,
        robust = TRUE
        )

    }else{

      cox <- survival::coxph(
        data = x[[i]],
        formula = surv_formula,
        weights = weights,
        robust = TRUE
        )

      }

    return(cox)


  }

  # loop cox_fit function over each imputed dataset
  cox_results_list <- lapply(
    X = 1:length(x),
    FUN = cox_fit
    )

  # Step 2: extract results into a format suitable for pooling
  # convert each model into a "mira" object (required for pooling)
  mira_object <- mice::as.mira(cox_results_list)

  # Step 3: pool the results using Rubin's rules
  pooled_results <- mice::pool(mira_object)

  # Step 4: format the pooled results
  cox_result <- broom::tidy(pooled_results, exponentiate = TRUE, conf.int = TRUE)

  return(cox_result)


}
