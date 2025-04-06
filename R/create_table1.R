#' Wrapper around \code{gtsummary::tbl_summary()} to create a beautiful Table 1 quickly
#'
#' @description
#' Builds a table 1 using the \code{gtsummary} package. The function takes a one-row-per-patient dataframe
#' and creates a summary table of covariates stratified by treatment group.
#'
#' @details
#' Wrapper for \code{\link[gtsummary]{tbl_summary}}.
#'
#' The function \code{create_table1} is a wrapper around the \code{gtsummary::tbl_summary()} function.
#' It is designed to create a summary table of covariates stratified by treatment group.
#' The function requires a one-row-per-patient dataframe and creates a summary table of covariates stratified by treatment group.
#'
#' @param x dataframe with individual-level patient data in a one-row-per-patient format with treatment stratification variable and covariates to be displayed in the Table 1
#' @param covariates character vector of columns/covariate names to be displayed in Table 1
#' @param covariates_labels named character vector or list of formulas specifying variables labels of covariate-label pairs to display in table
#' @param treat character specifying column name of treatment variable
#' @param explicit_na_categorical logical, should missings in categorical variables be explicitly included as a separate category (default is TRUE)
#'
#' @return object of class "tbl_summary" "gtsummary"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(encore.analytics)
#' # simulate a cohort with 1,000 patients with 20% missing data
#' data <- simulate_data(
#'   n = 1000,
#'   imposeNA = TRUE,
#'   propNA = 0.2
#'   )
#'
#' # create a Table 1
#' create_table1(
#'   x = data,
#'   covariates = c("dem_age_index_cont", "dem_sex_cont", "c_smoking_history"),
#'   covariates_labels = list(
#'     "dem_age_index_cont" = "Age",
#'     "dem_sex_cont" = "Sex",
#'     "c_smoking_history" = "Smoking history"
#'     ),
#'   treat = "treat"
#'   )
#'}
create_table1 <- function(x = NULL,
                          covariates = NULL,
                          covariates_labels = NULL,
                          treat = "treat",
                          explicit_na_categorical = TRUE
                          ){

  install_on_demand("cardx")
  install_on_demand("smd")

  # check input
  assertthat::assert_that(any(class(x) %in% c("data.frame", "tibble")), msg = "<x> is not a data.frame or tibble")
  assertthat::assert_that(all(covariates %in% colnames(x)), msg = "some or all of <covariates> not in <x>")
  assertthat::assert_that(is.list(covariates_labels) | is.null(covariates_labels), msg = "<covariates_labels> needs to be a named character vector or list of formulas specifying variables labels")
  assertthat::assert_that(length(covariates_labels) == length(covariates) | is.null(covariates_labels), msg = "<covariates_labels> must be a named character vector or list of formulas specifying variables labels")
  assertthat::assert_that(length(treat) == 1, msg = "<treat> must be a single character string")
  assertthat::assert_that(treat %in% colnames(x), msg = "<treat> not in <x>")
  assertthat::assert_that(is.logical(explicit_na_categorical), msg = "<explicit_na_categorical> must be a logical TRUE or FALSE")
  assertthat::assert_that(length(covariates) > 0, msg = "<covariates> must be a non-empty character vector")

  # assign treatment variable
  # and subset columns
  x[["strata"]] <- x[[treat]]
  x <- x[c(covariates, "strata")]

  if(explicit_na_categorical){

    # encode NA explicitly as "Missing" (only applicable to categorical)
    x <- x |>
      # logicals into categorical
      dplyr::mutate(
        dplyr::across(
          dplyr::where(function(.x) is.logical(.x)),
          .fns = function(.x) factor(.x, levels = c("TRUE", "FALSE", "Missing"))
          )
        ) |>
      # for all categorical, assign a "Missing" category
      dplyr::mutate(
        dplyr::across(
          dplyr::where(function(.x) is.character(.x) | is.factor(.x)),
          .fns = function(.x) forcats::fct_na_value_to_level(.x, level = "Missing")
          )
        ) |>
      # drop "Missing" level for covariates which are fully observed/have no NA
      droplevels()
  }

  table1 <- x |>
    gtsummary::tbl_summary(
      by = "strata",
      missing = "ifany",
      missing_text = "Missing",
      label = covariates_labels
      ) |>
    gtsummary::add_difference(test = dplyr::everything() ~ "smd") |>
    gtsummary::modify_column_hide(conf.low) |>
    gtsummary::add_overall() |>
    gtsummary::modify_header(
      label ~ "**Patient characteristic**",
      stat_0 ~ "**Total** <br> N = {N}",
      stat_1 ~ "**{level}** <br> N = {n} <br> ({style_percent(p, digits=1)}%)",
      stat_2 ~ "**{level}** <br> N = {n} <br> ({style_percent(p, digits=1)}%)"#,
      #test_result ~ "**SMD**"
      ) |>
    gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment received**")

  return(table1)

}
