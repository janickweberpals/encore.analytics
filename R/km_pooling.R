#' Pooled Kaplan-Meier estimate and survival curve
#'
#' @description
#' Computes pooled median survival Kaplan-Meier estimates using Rubin's rule
#' and outputs corresponding Kaplan-Meier curve across imputed and
#' matched/weighted datasets
#'
#' @details
#' The function requires an object of class mimids or wimids, which is the output
#' of a workflow that requires imputing multiple (m) datasets using mice or amelia
#' and matching or weighting each imputed dataset via the MatchThem package
#' (see examples).
#'
#' The function fits the pre-specified survfit model (\code{surv_formula}, \code{\link[survival]{survfit}} package)
#' to compute survival probabilities at each individual time point according to the Kaplan-Meier method.
#' For matched and weighted datasets, weights, cluster membership (matching only) and robust
#' variance estimates are considered in the \code{\link[survival]{survfit}} call by default.
#'
#' Since survival probabilities typically don't follow normal distributions,
#' these need to be transformed to approximate normality first before pooling
#' across imputed datasets and time points. To that end, survival probabilities are first
#' transformed using a complementary log-log transformation (\code{log(-log(1-pr(surv)))})
#' as recommended by multiple sources (Marshall, Billingham, and Bryan (2009)).
#'
#' To pool the transformed estimates across imputed datasets and time points, the pool.scalar
#' function is used to apply Rubin's rule to combine pooled estimates (qbar) according to
#' formula (3.1.2) Rubin (1987) and to compute the corresponding total variance (t) of the pooled
#' estimate according to formula (3.1.5) Rubin (1987).
#'
#' The pooled survival probabilities are then back-transformed via \code{1-exp(-exp(qbar))}
#' for pooled survival probability estimates and \code{1-exp(-exp(qbar +/- 1.96*sqrt(t)))}
#' for lower and upper 95% confidence intervals. As the formula indicates, the pooled standard
#' error is computed as the square root of the total variance. The vertically stacked table
#' with transformed and backtransformed estimates is returned with the
#' \code{km_survival_table} table.
#'
#' Finally, the median survival time is extracted from the \code{km_survival_table} table
#' by determining the time the survival probability drops below .5 for the first time.
#' For this a sub-function of Terry M. Therneau's \code{\link[survival]{print.survfit}} function
#' is used. Therneau also considers some edge cases/nuisances (x = time, y = surv):
#' - Nuisance 1: if one of the y's is exactly .5, we want the mean of the corresponding x and the first x for which y<.5. We need to use the equivalent of all.equal to check for a .5 however: survfit(Surv(1:100)~1) gives a value of .5 + 1.1e-16 due to roundoff error.
#' - Nuisance 2: there may by an NA in the y's
#' - Nuisance 3: if no y's are <=.5, then we should return NA
#' - Nuisance 4: the obs (or many) after the .5 may be censored, giving a stretch of values = .5 +- epsilon
#'
#' The function follows the following logic:
#' 1. Fit Kaplan-Meier survival function to each imputed and matched/weighted dataset
#' 2. Transform survival probabilities using complementary log-log transformation
#' 3. Pool transformed survival probabilities and compute total variance using Rubin's rule
#' 4. Back-transform pooled survival probabilities and compute 95% confidence intervals
#' 5. Extract median survival time and corresponding 95% confidence intervals
#' 6. Plot Kaplan-Meier curve with pooled survival probabilities and confidence intervals
#'
#' More references:
#' -  https://stefvanbuuren.name/fimd/sec-pooling.html
#' -  https://link.springer.com/article/10.1007/s10198-008-0129-y
#' -  https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-015-0048-4
#'
#' @seealso
#' \code{\link[survival]{survfit}} \code{\link[mice]{pool.scalar}} \code{\link[MatchThem]{matchthem}} \code{\link[MatchThem]{weightthem}}
#'
#' @param object imputed and matched (mimids) or weighted (wimids) object
#' @param surv_formula specification of survival model formula to be fitted
#'
#' @return list with pooled median survival estimate and pooled Kaplan-Meier curve
#' \code{km_median_survival}:
#' -  strata = stratum
#' -  t_median = median survival time
#' -  t_lower = lower 95% CI of median survival time
#' -  t_upper = upper 95% CI of median survival time
#'
#' \code{km_survival_table}:
#' -  strata = stratum
#' -  time = observed time point
#' -  m = number of imputed datasets
#' -  qbar = pooled univariate estimate of the complementary log-log transformed survival probabilities, see formula (3.1.2) Rubin (1987)
#' -  t = total variance of the pooled univariate estimate of the complementary log-log transformed survival probabilities, formula (3.1.5) Rubin (1987)
#' -  se = total standard error of the pooled estimate (derived as sqrt(t))
#' -  surv = back-transformed pooled survival probability
#' -  lower = Wald-type lower 95% confidence interval of back-transformed pooled survival probability
#' -  upper = Wald-type upper 95% confidence interval of back-transformed pooled survival probability
#'
#' \code{km_plot}: ggplot2 object with Kaplan-Meier curve
#'
#' @md
#'
#' @export
#'
#' @examples
#' if(require("MatchThem")){
#'
#'   library(encore.analytics)
#'   library(mice)
#'   library(MatchThem)
#'
#'   # simulate a cohort with 1,000 patients with 20% missing data
#'   data <- simulate_data(
#'     n = 1000,
#'     imposeNA = TRUE,
#'     propNA = 0.2
#'     )
#'
#'   # impute the data
#'   set.seed(42)
#'   mids <- mice(data, m = 5, print = FALSE)
#'
#'   # fit a propensity score model
#'   fit <- as.formula(treat ~ dem_age_index_cont + dem_sex_cont + c_smoking_history)
#'
#'   # weight (or alternatively match) patients within each imputed dataset
#'   wimids <- weightthem(
#'     formula = fit,
#'     datasets = mids,
#'     approach = "within",
#'     method = "glm",
#'     estimand = "ATO"
#'     )
#'
#'   # fit a survival model
#'   km_fit <- as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat)
#'
#'   # estimate and pool median survival times and Kaplan-Meier curve
#'   km_out <- km_pooling(
#'     object = wimids,
#'     surv_formula = km_fit
#'     )
#'
#'   # median survival time
#'   km_out$km_median_survival
#'
#'   # KM curve
#'   km_out$km_plot
#'
#' }
#'

km_pooling <- function(object = NULL,
                       surv_formula = stats::as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat)
                       ){

  # input checks
  # check if object is a mimids or wimids object
  assertthat::assert_that(inherits(object, c("mimids", "wimids")), msg = "<object> needs to be a mimids or wimids object")
  # check if surv_formula is a formula
  assertthat::assert_that(inherits(surv_formula, "formula"), msg = "<surv_formula> needs to be a formula")
  # check weights and subclass
  assertthat::assert_that("weights" %in% names(MatchThem::complete(object)), msg = "<object> needs to contain a weights column")
  # check if weights and subclass are in the data
  if(inherits(object, "mimids")){
    assertthat::assert_that("subclass" %in% names(MatchThem::complete(object)), msg = "<object> needs to contain a weights column")
  }

  # Fit Kaplan Meier --------------------------------------------------------

  # first, we fit a survival function for ith dataset
  compute_km <- function(i){

    # compute the survival function for dataset i

    ## for weighting
    if(inherits(object, "wimids")){

      survfit_fit <- survival::survfit(
        formula = surv_formula,
        weights = weights,
        robust = T,
        data = i
        )

    }

    ## for matching
    if(inherits(object, "mimids")){

      survfit_fit <- survival::survfit(
        formula = surv_formula,
        weights = weights,
        cluster = subclass,
        robust = T,
        data = i
        )

    }

    return(survfit_fit)

  }


  # compute KM across all imputed (and weighted/matched) datasets --------------------

  # next, we create a list of datasets to apply the compute_km function to
  # action = "all"  : produces a mild object (list) of the multiply imputed datasets
  # all = FALSE     : do NOT include observations with a zero estimated weight
  # include = FALSE : do NOT include original data with the missing values

  # Important: the all = parameter needs to be set to FALSE
  # since otherwise unmatched patients or those zero weight
  # will be included, too!
  object_list <- MatchThem::complete(object, action = "all", all = FALSE, include = FALSE)

  # now apply the compute_km function across all imputed and matched/weighted datasets
  # this creates a list with computed survival probabilities
  survival_fit_list <- lapply(object_list, FUN = compute_km)


  # pool survival probabilities ---------------------------------------------

  # now to combine/pool quantities with non-normal distributions
  # such as survival probabilities, Stef van Buuren
  # recommends to tansform survival probabilities
  # using a complementary log-log transformation
  # https://stefvanbuuren.name/fimd/sec-pooling.html
  # reference: Marshall, Billingham, and Bryan (2009) https://link.springer.com/article/10.1007/s10198-008-0129-y
  # reference: https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-015-0048-4

  # that is, the function cloglog performs the
  # transformations on all survival probabilities
  # stored in survival_fit_list
  cloglog <- function(i){

    # convert to data.frame
    survival_times_df <- with(
      summary(i),
      data.frame(strata, time, surv, std.err, lower, upper)
      ) |>
      dplyr::mutate(
        # compute transformed qbar
        q = log(-log(1-surv)),
        # compute transformed U
        u = std.err^2 / ((1-surv) * log(1-surv))^2
        )

    }

  # we apply this function to all fitted survival curves
  km_survival <- lapply(survival_fit_list, FUN = cloglog) |>

    # the resulting list with q and u per stratum and dataset
    # is then vertically stacked to a long data frame (using bind_rows).
    dplyr::bind_rows() |>

    # group by the exposure levels (stratum) and apply the pool.scalar function
    # which pools univariate estimates of m datasets. This returns:
    # m = The number of imputations
    # qbar = The pooled univariate estimate, formula (3.1.2) Rubin (1987)
    # t = The total variance of the pooled estimated, formula (3.1.5) Rubin (1987).
    dplyr::group_by(strata, time) |>
    dplyr::summarize(
      data.frame(mice::pool.scalar(Q = q, U = u, k = 1)[c("m", "qbar", "t")]),
      .groups = "drop"
      ) |>

    # we back-transform back and compute CI's
    dplyr::mutate(
      surv = 1-exp(-exp(qbar)),
      se = sqrt(t),
      lower = 1-exp(-exp(qbar - 1.96*se)),
      upper = 1-exp(-exp(qbar + 1.96*se))
      ) |>

    # arrange by strata and descending (!) survival probability
    dplyr::arrange(strata, desc(surv))


  # Plot a pooled Kaplan-Meier curve ----------------------------------------

  # plot pooled Kaplan-Meier curve based on pooled survival probabilities

  km_plot <- km_survival |>
    ggplot2::ggplot(ggplot2::aes(x = time, y = surv, color = strata, fill = strata)) +
    ggplot2::geom_line(linewidth = 1.25) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.2) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      x = "Follow-up time",
      y = "Pr(Survival) [%]",
      color = "Exposure",
      fill = "Exposure"
      ) +
    ggplot2::theme(legend.position = "top")


  # Extract median survival time --------------------------------------------

  # This section is taken from Terry M. Therneau's
  # print.survfit function in the survival package!

  # to extract the median survival times, we
  # apply the same sub-functions used in
  # print.survfit function of the original survival package
  # which defines the median survival time as the time
  # the survival probability drops below .5 for the first time
  # see https://github.com/cran/survival/blob/master/R/print.survfit.R#L109

  # Therneau defined some edge cases (x = time, y = surv):
  # Nuisance 1: if one of the y's is exactly .5, we want the mean of
  #    the corresponding x and the first x for which y<.5.  We need to
  #    use the equivalent of all.equal to check for a .5 however:
  #    survfit(Surv(1:100)~1) gives a value of .5 + 1.1e-16 due to
  #    roundoff error.
  # Nuisance 2: there may by an NA in the y's
  # Nuisance 3: if no y's are <=.5, then we should return NA
  # Nuisance 4: the obs (or many) after the .5 may be censored, giving
  #   a stretch of values = .5 +- epsilon
  minmin <- function(y, x){
    tolerance <- .Machine$double.eps^.5   #same as used in all.equal()
    keep <- (!is.na(y) & y <(.5 + tolerance))
    if (!any(keep)) NA
    else {
      x <- x[keep]
      y <- y[keep]
      if (abs(y[1]-.5) <tolerance  && any(y< y[1]))
        (x[1] + x[min(which(y<y[1]))])/2
      else x[1]
    }
  }

  # apply this across strata listed in the km_survival table
  minmin_strata <- function(i){

    km_survival_stratum <- km_survival |>
      dplyr::filter(strata == i)

    # median survival time
    med <- minmin(y = km_survival_stratum$surv, km_survival_stratum$time)

    # 95% CI of median survival time
    # = time when lower and upper
    # 95% confidence intervals of the survival
    # probability drop below .5
    if(!is.null(km_survival_stratum$upper)){
      upper <- minmin(y = km_survival_stratum$upper, x = km_survival_stratum$time)
      lower <- minmin(y = km_survival_stratum$lower, x = km_survival_stratum$time)
      minmin_strata_return <- c(med, lower, upper)

    }else{

      # Therneau assigns a 0 of no confidence intervals are available
      upper <- 0
      lower <- 0

    }

    # provide output table
    minmin_strata_return_tibble <- tibble::tibble(
      strata = i, # stratum
      t_median = med, # median survival time
      t_lower = lower, # lower 95% CI of median survival time
      t_upper = upper # upper 95% CI of median survival time
      )

  }

  km_median_survival <- do.call(rbind, lapply(X = unique(km_survival$strata), FUN = minmin_strata))


  # return results ----------------------------------------------------------

  return(
    list(
      km_median_survival = km_median_survival,
      km_plot = km_plot,
      km_survival_table = km_survival
      )
    )


}
