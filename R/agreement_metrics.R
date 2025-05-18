

#' Calculate Agreement Metrics Between RCT and RWE Results
#'
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
      "Abbreviations: CI = Confidence interval, HR = Hazard ratio, LoT = line of therapy, RCT = Randomized controlled trial, RWE = Real-world evidence, SMD = standardized mean difference (based on log hazard ratios)")

  return(x_gt)
}
