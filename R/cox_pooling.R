#' Manually fit and pool Cox proportional hazards model results from multiple imputed datasets
#'
#' @description
#' This function manually fits and pools results from Cox proportional hazards models using
#' a list of imputed datasets. It leverages the `mice::as.mira` and `mice::pool` functions to
#' ensure proper pooling of results across multiple imputations using Rubin's rules.
#'
#' @details
#' The function takes the imputed datasets (\code{x}) as a list of data frames, or as a
#' mimids, wimids or mids object (see the sections "Input types and default model arguments" and
#' "Precedence of arguments in ..." below), as well as a formula for the Cox proportional hazards model.
#' Weights and clusters of lists of data frames are specified explicitly via \code{...}, e.g.,
#' \code{weights = weights} for weighted data and \code{weights = weights, cluster = subclass}
#' for matched data. A list is particularly useful when working with imputed datasets that are not in the
#' form of \code{mimids} or \code{wimids} objects, such as when intermediate steps like raking weights
#' (via \code{raking_weights}) are applied. Arguments of the \code{\link[survival]{coxph}} call
#' can be supplied via \code{...}, e.g., \code{ties = "breslow"}.
#'
#' \strong{Breaking change in 0.4.0}: previously, a list of data frames had to contain a column named
#' \code{weights}, which was used automatically, as was a column named \code{subclass} (cluster
#' membership of matched datasets). Weights and clusters are no longer applied automatically to lists, in line
#' with \code{\link{km_pooling}}. Update calls such as \code{cox_pooling(x, surv_formula)} to
#' \code{cox_pooling(x, surv_formula, weights = weights)} (weighted data) or
#' \code{cox_pooling(x, surv_formula, weights = weights, cluster = subclass)} (matched data).
#' Calls that are not updated fit an unweighted model and raise a warning that names the unused columns.
#' mimids and wimids objects are not affected.
#'
#' The function follows these steps:
#' 1. Fit a Cox proportional hazards model to each imputed dataset, using the weights and cluster
#'    (if any) of the input type or specified via \code{...}.
#' 2. Convert the list of fitted models into a \code{mira} object using \code{mice::as.mira}.
#' 3. Pool the results using \code{mice::pool}, which applies Rubin's rules for combining estimates
#'    and variances across imputations.
#' 4. Format the pooled results, including exponentiating the hazard ratios and calculating confidence intervals.
#'
#' @seealso
#' \code{\link{km_pooling}}, \code{\link{raking_weights}}, \code{\link[survival]{coxph}}, \code{\link[mice]{pool}},
#' \code{\link[mice]{as.mira}}, \code{\link[MatchThem]{matchthem}}, \code{\link[MatchThem]{weightthem}}
#'
#' @inheritParams pooling_inputs
#' @param surv_formula A formula for the Cox proportional hazards model (default is \code{Surv(fu_itt_months, death_itt) ~ treat}).
#'
#' @inheritSection pooling_inputs Input types and default model arguments
#' @inheritSection pooling_inputs Precedence of arguments in \code{...}
#' @inheritSection pooling_inputs Transparency
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
#' # Define a survival model formula
#' cox_fit <- as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat)
#'
#' # 1) Weighted (wimids) or matched (mimids) object:
#' # weights (and clusters) are set automatically
#' cox_pooling(wimids, surv_formula = cox_fit)
#'
#' # 2) List of data frames: weights are specified explicitly via `...`
#' # (all = FALSE excludes observations with zero weight)
#' wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
#' cox_pooling(wimids_list, surv_formula = cox_fit, weights = weights)
#'
#' # for matched datasets, additionally specify the matched sets as clusters:
#' # cox_pooling(matched_list, surv_formula = cox_fit, weights = weights, cluster = subclass)
#'
#' # 3) Imputed (mids) object: unweighted Cox model
#' cox_pooling(mids, surv_formula = cox_fit)
#'
#' # 4) Pass additional arguments to the underlying coxph() call
#' cox_pooling(wimids, surv_formula = cox_fit, ties = "breslow")
cox_pooling <- function(x,
                        surv_formula = stats::as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat),
                        ...
                        ){

  # input checks----

  # check if surv_formula is a formula
  assertthat::assert_that(inherits(surv_formula, "formula"), msg = "<surv_formula> needs to be a formula")

  # bring the input into the form of a list of datasets plus the default
  # model arguments of the input type (weights, cluster, ...)
  input <- prepare_pooling_input(x)

  # check that ... does not try to override arguments set internally
  # (...names() reads argument names without forcing/evaluating them, since
  # column names such as `id = patientid` are resolved in each dataset later)
  reserved_dots <- intersect(...names(), c("formula", "data"))
  assertthat::assert_that(
    length(reserved_dots) == 0,
    msg = paste0(
      "arguments passed via `...` must not include arguments that are set internally by cox_pooling(): ",
      paste(reserved_dots, collapse = ", ")
    )
  )

  # capture the *unevaluated* expressions behind `...` (e.g. id = patientid)
  user_args <- as.list(substitute(list(...)))[-1]

  # Step 1: fit Cox model to each imputed dataset----
  # merging of default and user arguments, checks, the console message and the
  # construction of the coxph() call are handled in fit_models()
  cox_results_list <- fit_models(
    input = input,
    fit_fun = quote(survival::coxph),
    model_label = "Cox proportional hazards model",
    formula = surv_formula,
    user_args = user_args
  )$fits

  # Step 2: extract results into a format suitable for pooling
  # convert each model into a "mira" object (required for pooling)
  mira_object <- mice::as.mira(unname(cox_results_list))

  # Step 3: pool the results using Rubin's rules
  pooled_results <- mice::pool(mira_object)

  # Step 4: format the pooled results
  cox_result <- broom::tidy(pooled_results, exponentiate = TRUE, conf.int = TRUE)

  return(cox_result)

}
