#' Manually fit and pool Cox proportional hazards model results from multiple imputed datasets
#'
#' @description
#' Function manually fits and pools results from Cox proportional hazards models using
#' a list of imputed datasets.
#'
#' @details The function requires an list of imputed data frames with weights and cluster (matching) information
#' and a formula for the Cox proportional hazards model. The data frames must have a column names "weights"
#' and "subclass" (for matched datasets to indicate the cluster membership).
#'
#' This is a convenience wrapper for fitting Cox models to multiple imputed datasets
#' which do not come as a \code{mimids} or \code{wimids} object. This is useful when
#' there are intermediate steps in the analysis pipeline, such as computing raking weights
#' via \code{\link[encore.analytics]{raking_weights} which are so far not straightforward
#' to implement in a \code{mimids} or \code{wimids} object.
#'
#' The function follows the following logic:
#' 1. Fit a Cox proportional hazards model to each imputed dataset; if a \code{subclass} column is present in the data, it is used as a cluster variable for matched pairs
#' 2. Pool the results using the \code{pool.scalar} function from the mice package, that is, univariate estimates (qbar) are pooled via formula (3.1.2) Rubin (1987) and the total variance (t) is estimated via robust standardard errors according to formula (3.1.5) Rubin (1987).
#' 3. Compute the confidence intervals and exponentiate the results via exp(qbar +/- 1.96 * sqrt(t))
#'
#' @seealso
#' \code{\link[survival]{coxph}} \code{\link[mice]{pool.scalar}} \code{\link[MatchThem]{matchthem}} \code{\link[MatchThem]{weightthem}}
#'
#' @param x list of imputed datasets with weights or raking weights (\code{\link[encore.analytics]{raking_weights}}) and cluster (matched datasets only)
#' @param surv_formula formula for the Cox proportional hazards model (default is \code{Surv(fu_itt_months, death_itt) ~ treat})
#'
#' @return list of data frames with updated raking weights (\code{raking_weights})
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
#' # simulate a cohort with 1,000 patients with 20% missing data
#' data <- simulate_data(
#'  n = 500,
#'  imposeNA = TRUE,
#'  propNA = 0.2
#'  )
#'
#' # impute the data
#' set.seed(42)
#' mids <- mice(data, m = 5, print = FALSE)
#'
#' # fit a propensity score model
#' fit <- as.formula(treat ~ dem_age_index_cont + dem_sex_cont + c_smoking_history)
#'
#' # weight (or alternatively match) patients within each imputed dataset
#' wimids <- weightthem(
#'  formula = fit,
#'  datasets = mids,
#'  approach = "within",
#'  method = "glm",
#'  estimand = "ATO"
#'  )
#'
#' # create a list of imputed and weighted datasets
#' wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
#'
#' # fit a survival model
#' cox_fit <- as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat)
#'
#' # fit and pool Cox proportional hazards model results
#' cox_pooling(wimids_list, surv_formula = cox_fit)
#'
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

  # fit Cox model to each imputed dataset----
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

    # bring into tidy data.frame format
    cox_return <- cox |>
      broom::tidy(
        exponentiate = FALSE,
        conf.int = TRUE
        )

    return(cox_return)


  }

  # loop cox_fit function over each imputed dataset
  cox_results_list <- lapply(
    X = 1:length(x),
    FUN = cox_fit
    )

  # pool results using pool.scalar function
  cox_result <- cox_results_list |>

    # the resulting list with estimate (Q) and robust se (U) dataset
    # is then vertically stacked to a long data frame (using bind_rows).
    dplyr::bind_rows() |>

    # apply the pool.scalar function
    # which pools univariate estimates of m datasets. This returns:
    # m = The number of imputations
    # qbar = The pooled univariate estimate, formula (3.1.2) Rubin (1987)
    # t = The total variance of the pooled estimated, formula (3.1.5) Rubin (1987).
    dplyr::summarize(
      data.frame(mice::pool.scalar(Q = estimate, U = robust.se, k = 1)[c("m", "qbar", "t")])
    ) |>
    # compute confidence intervals and exponentiate
    dplyr::transmute(
      hr = exp(qbar),
      lci = exp(qbar - 1.96 * sqrt(t)),
      uci = exp(qbar + 1.96 * sqrt(t))
      )

  return(cox_result)


}
