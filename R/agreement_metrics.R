# results table for example
x <- tibble::tribble(
  ~Analysis, ~Database, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
  "Main analysis", "Database 1", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87,
  "Main analysis", "Database 2", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2,
  "Main analysis", "Database 3", 0.8, 0.7, 0.9, 1.5, 1.4, 1.6
  )

agreement_metrics <- function(x,
                              analysis_col,
                              group_col = NULL, # subgroup, e.g, database
                              estimate_label = "HR (95% CI)"
                              ){

  # input checks
  assertthat::assert_that(any(class(x) %in% c("data.frame", "tibble")), msg = "<x> is not a data.frame or tibble")
  assertthat::assert_that(all(c("rct_estimate", "rct_lower", "rct_upper", "rwe_estimate", "rwe_lower", "rwe_upper") %in% colnames(x)), msg = "<x> does not contain all required columns")
  assertthat::assert_that(!is.null(analysis_col), msg = "<analysis_col> not in <x>")
  assertthat::assert_that(is.null(group_col) | group_col %in% colnames(x), msg = "<group_col> not in <x>")

  # if no smd_value present in the results table, call smd_agreement
  # function and compute the smd_value for each row
  if(!"smd_value" %in% colnames(x)){

    x <- x |>
      # compute smd for each row
      dplyr::rowwise() |>
      dplyr::mutate(
        smd_value = smd_agreement(
          rct_estimate = log(rct_estimate),
          rct_lower = log(rct_lower),
          rct_upper = log(rct_upper),
          rwe_estimate = log(rwe_estimate),
          rwe_lower = log(rwe_lower),
          rwe_upper = log(rwe_upper)
          )
        ) |>
      dplyr::ungroup()

    }

  # now compute the binary agreement metrics:
  # significance agreement = agreement between RCT and emulated trial treatment effect with regards to directionality and statistical significance.
  # estimate agreement = agreement that the estimated RWE treatment effect is within the 95% CI of the RCT treatment effect estimate.
  # smd agreement = agreement that the standardized mean difference (SMD) is less than 1.96
  x <- x |>
    dplyr::mutate(
      # significance agreement
      significance_agreement = ifelse(
        # superiority
        rct_estimate < 1 & rct_upper < 1 & rwe_estimate < 1 & rwe_upper < 1 |
          # non-inferiority
          rct_estimate < 1 & rct_upper >= 1 & rwe_estimate < 1 & rwe_upper >= 1 |
          # inferiority (made up)
          rct_estimate >= 1 & rct_lower >= 1 & rwe_estimate >= 1 & rwe_upper >= 1,
        "Yes", "No"
        ),

      # estimate agreement
      estimate_agreement = ifelse(
        rwe_estimate >= rct_lower & rwe_estimate <= rct_upper, "Yes", "No"
        ),

      # smd agreement
      smd_agreement = ifelse(
        abs(smd_value) < 1.96, "Yes", "No"
        )
      )

  # format and bring in shape before converting
  # x into a gt table object
  x_format <- x |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) format(x, digits = 2, nsmall = 2))) |>
    dplyr::mutate(
      RCT = glue::glue("{rct_estimate} ({rct_lower} - {rct_upper})"),
      RWE = glue::glue("{rwe_estimate} ({rwe_lower} - {rwe_upper})"),
      smd_agreement = glue::glue("{smd_agreement} ({smd_value})")
      ) |>
    dplyr::select(dplyr::all_of(c(analysis_col, group_col)), "RCT", "RWE", "significance_agreement", "estimate_agreement", "smd_agreement")

  # if group_col is not NULL, then we need to group by it
  x_format <- x_format |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col)))

  # now convert and format gt table
  x_gt <- x_format |>
    gt::gt() |>
    gt::tab_spanner(
      label = estimate_label,
      columns = c(RCT, RWE)
      ) |>
    gt::cols_label(
      significance_agreement = gt::md("Statistical <br> significance <br> agreement"),
      estimate_agreement = gt::md("Estimate <br> agreement"),
      smd_agreement = "SMD"
      ) |>
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
    gt::tab_style(
      style = gt::cell_text(color = "darkgreen"),
      locations = gt::cells_body(
        columns = significance_agreement,
        rows = significance_agreement == "Yes"
        )
      ) |>
    gt::tab_style(
      style = gt::cell_text(color = "darkred"),
      locations = gt::cells_body(
        columns = significance_agreement,
        rows = significance_agreement == "No"
        )
      ) |>
    gt::tab_style(
      style = gt::cell_text(color = "darkgreen"),
      locations = gt::cells_body(
        columns = estimate_agreement,
        rows = estimate_agreement == "Yes"
        )
      ) |>
    gt::tab_style(
      style = gt::cell_text(color = "darkred"),
      locations = gt::cells_body(
        columns = estimate_agreement,
        rows = estimate_agreement == "No"
        )
      ) |>
    gt::tab_style(
      style = gt::cell_text(color = "darkgreen"),
      locations = gt::cells_body(
        columns = smd_agreement,
        rows = stringr::str_detect(smd_agreement, "Yes")
        )
      ) |>
    gt::tab_style(
      style = gt::cell_text(color = "darkred"),
      locations = gt::cells_body(
        columns = smd_agreement,
        rows = stringr::str_detect(smd_agreement, "No")
        )
      ) |>
    gt::tab_footnote("Abbreviations: CI = Confidence interval, HR = Hazard ratio, LoT = line of therapy, RCT = Randomized controlled trial, RWE = Real-world evidence, SMD = standardized mean difference (based on log hazard ratios)")

  return(x_gt)

}
