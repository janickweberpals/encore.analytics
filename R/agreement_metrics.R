#' Calculate Agreement Metrics Between RCT and RWE Results
#'
#' @description Creates a formatted table comparing results from randomized controlled trials (RCTs)
#' with real-world evidence (RWE) emulation studies by calculating agreement metrics.
#'
#' @details
#' Calculates three types of agreement metrics:
#' - Statistical significance agreement: Same direction and statistical significance
#' - Estimate agreement: RWE estimate within RCT confidence interval
#' - SMD agreement: Standardized mean difference below threshold (default 1.96)
#'
#' All estimates should be hazard ratios (HR) and must be positive.
#' Estimates are log-transformed for SMD calculation.
#'
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom smd smd
#'
#' @param x A data.frame or tibble containing RCT and RWE results (in wide format, see examples).
#' Must contain these columns:
#'
#' - rct_estimate = Point estimate from the RCT
#' - rct_lower = Lower confidence limit of the RCT result
#' - rct_upper = Upper confidence limit of the RCT result
#' - rwe_estimate = Point estimate from the RWE emulation study
#' - rwe_lower = Lower confidence limit of the RWE result
#' - rwe_upper = Upper confidence limit of the RWE result
#'
#' @param analysis_col Character. Name of column identifying different analyses
#' @param group_col Character. Optional. Name of column for grouping results (e.g., database)
#' @param estimate_label Character. Label for estimates in table header. Default: "HR (95% CI)"
#' @param smd_threshold Numeric. Threshold for SMD agreement. Default: 1.96 (α=0.05)
#'
#' @return A gt table object with formatted agreement metrics
#'
#' @md
#'
#' @examples
#' # Example 1: One analysis per database with grouping
#'
#'x <- tibble::tribble(
#'~Analysis, ~Database, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
#'"Main analysis", "Database 1", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87,
#'"Main analysis", "Database 2", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2,
#'"Main analysis", "Database 3", 0.8, 0.7, 0.9, 1.5, 1.4, 1.6
#')
#'
#' agreement_metrics(x, analysis_col = "Analysis", group_col = "Database")
#'
#' # Example 2:  Three analyses, one database without grouping
#'
#' x <- tibble::tribble(
#' ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
#' "Main analysis", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87,
#' "Sensitivity analysis 1", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2,
#' "Sensitivity analysis 2", 0.8, 0.7, 0.9, 1.5, 1.4, 1.6
#' )
#'
#' agreement_metrics(x, analysis_col = "Analysis")
#'
#' # Example 3: Three analyses per database with grouping
#' x <- tibble::tribble(
#' ~Analysis, ~Database, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
#' "Main analysis", "Database 1", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87,
#' "Main analysis", "Database 2", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2,
#' "Main analysis", "Database 3", 0.8, 0.7, 0.9, 1.5, 1.4, 1.6,
#' "Sensitivity analysis 1", "Database 1", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2,
#' "Sensitivity analysis 1", "Database 2", 0.8, 0.7, 0.9, 1.5, 1.4, 1.6,
#' "Sensitivity analysis 1", "Database 3", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87,
#' "Sensitivity analysis 2", "Database 1", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87,
#' "Sensitivity analysis 2", "Database 2", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2,
#' "Sensitivity analysis 2", "Database 3", 0.8, 0.7, 0.9, 1.5, 1.4, 1.6
#' )
#'
#' agreement_metrics(x, analysis_col = "Analysis", group_col = "Database")
#'
#' @export
agreement_metrics <- function(x,
                              analysis_col,
                              group_col = NULL, # subgroup, e.g, database
                              estimate_label = "HR (95% CI)",
                              smd_threshold = 1.96 # customizable threshold for SMD agreement
                              ){

  # input checks
  assertthat::assert_that(any(class(x) %in% c("data.frame", "tibble")), msg = "<x> is not a data.frame or tibble")
  assertthat::assert_that(all(c("rct_estimate", "rct_lower", "rct_upper", "rwe_estimate", "rwe_lower", "rwe_upper") %in% colnames(x)), msg = "<x> does not contain all required columns")
  assertthat::assert_that(!is.null(analysis_col), msg = "<analysis_col> cannot be NULL")
  assertthat::assert_that(analysis_col %in% colnames(x), msg = sprintf("<analysis_col> '%s' not found in <x>", analysis_col))
  if(!is.null(group_col)) assertthat::assert_that(group_col %in% colnames(x), msg = "<group_col> not in <x>")

  # check for non-positive values
  assertthat::assert_that(all(x$rct_estimate > 0), msg = "RCT estimates must be positive for hazard ratios")
  assertthat::assert_that(all(x$rwe_estimate > 0), msg = "RWE estimates must be positive for hazard ratios")

  # if no smd_value present in the results table, calculate it
  if(!"smd_value" %in% colnames(x)){
    x <- x |>
      dplyr::rowwise() |>
      dplyr::mutate(
        smd_value = tryCatch({
          smd_agreement(
            rct_estimate = log(rct_estimate),
            rct_lower = log(rct_lower),
            rct_upper = log(rct_upper),
            rwe_estimate = log(rwe_estimate),
            rwe_lower = log(rwe_lower),
            rwe_upper = log(rwe_upper)
          )
        }, error = function(e) {
          warning(sprintf("SMD calculation failed: %s", e$message))
          NA_real_
        })
      ) |>
      dplyr::ungroup()
  }

  # calculate agreement metrics
  x <- x |>
    dplyr::mutate(
      # significance agreement with additional categories
      significance_agreement = dplyr::case_when(
        # superiority
        rct_estimate < 1 & rct_upper < 1 & rwe_estimate < 1 & rwe_upper < 1 ~ "Yes",
        # non-inferiority
        rct_estimate < 1 & rct_upper >= 1 & rwe_estimate < 1 & rwe_upper >= 1 ~ "Yes",
        # inferiority
        rct_estimate >= 1 & rct_lower >= 1 & rwe_estimate >= 1 & rwe_upper >= 1 ~ "Yes",
        # all other cases
        TRUE ~ "No"
        ),

      # estimate agreement
      estimate_agreement = dplyr::if_else(
        rwe_estimate >= rct_lower & rwe_estimate <= rct_upper, "Yes", "No"
        ),

      # smd agreement using parameter
      smd_agreement = dplyr::case_when(
        is.na(smd_value) ~ "NA",
        abs(smd_value) < smd_threshold ~ "Yes",
        TRUE ~ "No"
        )
      )

  # format table for display
  x_format <- x |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ format(.x, digits = 2, nsmall = 2))) |>
    dplyr::mutate(
      RCT = glue::glue("{rct_estimate} ({rct_lower} - {rct_upper})"),
      RWE = glue::glue("{rwe_estimate} ({rwe_lower} - {rwe_upper})"),
      smd_agreement = glue::glue("{smd_agreement} ({smd_value})")
      ) |>
    dplyr::select(dplyr::all_of(c(analysis_col, group_col)), "RCT", "RWE", "significance_agreement", "estimate_agreement", "smd_agreement")

  # conditional grouping
  if (!is.null(group_col)) {
    x_format <- x_format |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_col)))
  }

  # create and format gt table
  x_gt <- x_format |>
    gt::gt() |>
    gt::tab_spanner(
      label = estimate_label,
      columns = c("RCT", "RWE")
      ) |>
    gt::cols_label(
      significance_agreement = gt::md("Statistical <br> significance <br> agreement"),
      estimate_agreement = gt::md("Estimate <br> agreement"),
      smd_agreement = "SMD"
      )

  # apply styles for agreement columns
  for (col in c("significance_agreement", "estimate_agreement", "smd_agreement")) {
    x_gt <- x_gt |>
      gt::tab_style(
        style = gt::cell_text(color = "darkgreen"),
        locations = gt::cells_body(
          columns = !!col,
          rows = if (col == "smd_agreement") {
            stringr::str_detect(x_format[[col]], "Yes")
          } else {
            x_format[[col]] == "Yes"
          }
        )
      ) |>
      gt::tab_style(
        style = gt::cell_text(color = "darkred"),
        locations = gt::cells_body(
          columns = !!col,
          rows = if (col == "smd_agreement") {
            stringr::str_detect(x_format[[col]], "No")
          } else {
            x_format[[col]] == "No"
          }
        )
      )
  }

  # footnote estimate label
  if(estimate_label == "HR (95% CI)") footnote_label <- "HR = Hazard ratio"

  # Add bold styling for headers
  x_gt <- x_gt |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
      ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_spanners()
      ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_row_groups()
      ) |>
    gt::tab_footnote(
      glue::glue("Abbreviations: CI = Confidence interval, {footnote_label}, RCT = Randomized controlled trial, RWE = Real-world evidence, SMD = standardized mean difference (based on log hazard ratios)")
      )

  return(x_gt)
}
