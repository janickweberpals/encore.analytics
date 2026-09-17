test_that("agreement_metrics handles basic input correctly", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Main analysis", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87
  )
  
  result <- agreement_metrics(x, analysis_col = "Analysis")
  expect_s3_class(result, "gt_tbl")
  expect_true("Analysis" %in% colnames(result[["_data"]]))
  expect_true(all(c("RCT", "RWE", "significance_agreement", "estimate_agreement", "smd_agreement") %in% 
                  colnames(result[["_data"]])))
})

test_that("agreement_metrics handles grouped data correctly", {
  x <- tibble::tribble(
    ~Analysis, ~Database, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Main", "DB1", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87,
    "Main", "DB2", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2
  )
  
  result <- agreement_metrics(x, analysis_col = "Analysis", group_col = "Database")
  expect_s3_class(result, "gt_tbl")
  expect_true(all(c("Analysis", "Database") %in% colnames(result[["_data"]])))
})

test_that("agreement_metrics calculates agreement metrics correctly", {
  # Test case 1: Perfect agreement
  x1 <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Perfect", 0.80, 0.70, 0.90, 0.80, 0.70, 0.90
  )
  result1 <- agreement_metrics(x1, analysis_col = "Analysis")
  expect_equal(result1[["_data"]][["significance_agreement"]], "Yes")
  expect_equal(result1[["_data"]][["estimate_agreement"]], "Yes")
  expect_true(stringr::str_detect(result1[["_data"]][["smd_agreement"]], "Yes"))
  
  # Test case 2: Complete disagreement
  x2 <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Disagree", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2
  )
  result2 <- agreement_metrics(x2, analysis_col = "Analysis")
  expect_equal(result2[["_data"]][["significance_agreement"]], "No")
  expect_equal(result2[["_data"]][["estimate_agreement"]], "No")
  expect_true(stringr::str_detect(result2[["_data"]][["smd_agreement"]], "No"))
})

test_that("agreement_metrics handles invalid inputs appropriately", {
  # Test missing required columns
  x_missing <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rwe_estimate,
    "Test", 0.87, 0.82
  )
  expect_error(
    agreement_metrics(x_missing, analysis_col = "Analysis"),
    "<x> does not contain all required columns"
  )
  
  # Test non-existent analysis column
  x_valid <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Test", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87
  )
  expect_error(
    agreement_metrics(x_valid, analysis_col = "NonExistent"),
    "not found in <x>"
  )
  
  # Test non-positive values
  x_negative <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Test", -0.87, -0.78, -0.97, 0.82, 0.76, 0.87
  )
  expect_error(
    agreement_metrics(x_negative, analysis_col = "Analysis"),
    "RCT estimates must be positive"
  )
})

test_that("agreement_metrics handles custom SMD threshold", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Test", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87
  )
  
  # With default threshold (1.96)
  result1 <- agreement_metrics(x, analysis_col = "Analysis")
  
  # With stricter threshold (1.0)
  result2 <- agreement_metrics(x, analysis_col = "Analysis", smd_threshold = .8)
  
  # Results should differ due to different thresholds
  smd1 <- stringr::str_detect(result1[["_data"]][["smd_agreement"]], "Yes")
  smd2 <- stringr::str_detect(result2[["_data"]][["smd_agreement"]], "Yes")
  expect_false(identical(smd1, smd2))
})

test_that("agreement_metrics preserves row order", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Analysis 3", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87,
    "Analysis 1", 0.50, 0.40, 0.60, 0.55, 0.45, 0.65,
    "Analysis 2", 0.70, 0.60, 0.80, 0.75, 0.65, 0.85
  )

  result <- agreement_metrics(x, analysis_col = "Analysis")
  expect_equal(
    result[["_data"]][["Analysis"]],
    c("Analysis 3", "Analysis 1", "Analysis 2")
  )
})

test_that("agreement_metrics does not error for unrecognized estimate_label and omits its abbreviation", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Main analysis", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87
  )

  # a label that is not one of "HR/OR/RR (95% CI)" must not raise an error
  result <- agreement_metrics(x, analysis_col = "Analysis", estimate_label = "aHR (95% CI)")

  footnote <- result[["_footnotes"]][["footnotes"]][[1]]
  expect_true(stringr::str_detect(footnote, "Abbreviations: CI = Confidence interval, RCT"))
  expect_false(stringr::str_detect(footnote, "aHR ="))
})

test_that("agreement_metrics adds the matching abbreviation for recognized estimate_label values", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Main analysis", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87
  )

  result_hr <- agreement_metrics(x, analysis_col = "Analysis", estimate_label = "HR (95% CI)")
  footnote_hr <- result_hr[["_footnotes"]][["footnotes"]][[1]]
  expect_true(stringr::str_detect(footnote_hr, "HR = Hazard ratio"))

  result_or <- agreement_metrics(x, analysis_col = "Analysis", estimate_label = "OR (95% CI)")
  footnote_or <- result_or[["_footnotes"]][["footnotes"]][[1]]
  expect_true(stringr::str_detect(footnote_or, "OR = Odds ratio"))

  result_rr <- agreement_metrics(x, analysis_col = "Analysis", estimate_label = "RR (95% CI)")
  footnote_rr <- result_rr[["_footnotes"]][["footnotes"]][[1]]
  expect_true(stringr::str_detect(footnote_rr, "RR = Rate ratio"))
})

test_that("agreement_metrics significance_agreement requires both RCT and RWE to be statistically significant", {
  # RCT is significantly harmful (lower bound > 1), but RWE's CI crosses the
  # null value (not statistically significant) -- these must NOT agree, even
  # though both point estimates are >= 1
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "RWE not significant", 1.5, 1.2, 1.8, 1.3, 0.7, 2.0
  )

  result <- agreement_metrics(x, analysis_col = "Analysis")
  expect_equal(result[["_data"]][["significance_agreement"]], "No")
})

test_that("agreement_metrics significance_agreement is symmetric for non-significant results in either direction", {
  # both non-significant with point estimates >= 1 (harmful direction) and
  # both non-significant with point estimates < 1 (protective direction)
  # must be classified as agreement ("Yes") consistently
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Both null, >= 1", 1.2, 0.9, 1.6, 1.3, 0.8, 1.9,
    "Both null, < 1", 0.8, 0.6, 1.1, 0.75, 0.5, 1.2
  )

  result <- agreement_metrics(x, analysis_col = "Analysis")
  expect_equal(result[["_data"]][["significance_agreement"]], c("Yes", "Yes"))
})

test_that("agreement_metrics significance_agreement agrees when both RCT and RWE are significantly harmful", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Both significantly harmful", 1.5, 1.2, 1.8, 1.4, 1.1, 1.7
  )

  result <- agreement_metrics(x, analysis_col = "Analysis")
  expect_equal(result[["_data"]][["significance_agreement"]], "Yes")
})

test_that("agreement_metrics metrics argument selects which columns are computed and displayed", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Main analysis", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87
  )

  result <- agreement_metrics(
    x,
    analysis_col = "Analysis",
    metrics = c("significance_agreement", "estimate_agreement")
  )
  expect_true(all(
    c("significance_agreement", "estimate_agreement") %in%
      colnames(result[["_data"]])
  ))
  expect_false("smd_agreement" %in% colnames(result[["_data"]]))

  footnote <- result[["_footnotes"]][["footnotes"]][[1]]
  expect_false(stringr::str_detect(footnote, "SMD ="))

  result_smd_only <- agreement_metrics(
    x,
    analysis_col = "Analysis",
    metrics = "smd_agreement"
  )
  expect_false(any(
    c("significance_agreement", "estimate_agreement") %in%
      colnames(result_smd_only[["_data"]])
  ))
  expect_true("smd_agreement" %in% colnames(result_smd_only[["_data"]]))
})

test_that("agreement_metrics skips the SMD calculation entirely when smd_agreement is excluded", {
  # degenerate RCT bounds (lower == upper) make the smd_agreement() helper
  # stop(), which agreement_metrics() converts into a warning + NA smd_value
  # only when smd_agreement is actually requested and thus computed
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Degenerate RCT bounds", 0.87, 0.87, 0.87, 0.82, 0.76, 0.87
  )

  expect_warning(
    agreement_metrics(x, analysis_col = "Analysis"),
    "SMD calculation failed"
  )

  expect_no_warning(
    agreement_metrics(
      x,
      analysis_col = "Analysis",
      metrics = c("significance_agreement", "estimate_agreement")
    )
  )
})

test_that("agreement_metrics validates the metrics argument", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Main analysis", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87
  )

  expect_error(
    agreement_metrics(x, analysis_col = "Analysis", metrics = "bogus"),
    "<metrics> must be a subset of"
  )
  expect_error(
    agreement_metrics(x, analysis_col = "Analysis", metrics = character(0)),
    "<metrics> must be a non-empty character vector"
  )
})

test_that("agreement_metrics orders metric columns canonically regardless of metrics input order", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Main analysis", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87
  )

  result <- agreement_metrics(
    x,
    analysis_col = "Analysis",
    metrics = c("estimate_agreement", "significance_agreement")
  )
  metric_cols <- intersect(
    colnames(result[["_data"]]),
    c("significance_agreement", "estimate_agreement", "smd_agreement")
  )
  expect_equal(metric_cols, c("significance_agreement", "estimate_agreement"))
})

test_that("agreement_metrics show_aggregate_total is on by default and shows the correct pooled percentage", {
  # "Perfect" agrees on all metrics (Yes/Yes/Yes), "Disagree" agrees on none
  # (No/No/No) -> pooled across all 3 metrics and 2 rows = 3 Yes / 6 total
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Perfect", 0.80, 0.70, 0.90, 0.80, 0.70, 0.90,
    "Disagree", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2
  )

  result <- agreement_metrics(x, analysis_col = "Analysis")
  expect_length(result[["_source_notes"]], 1)
  expect_true(stringr::str_detect(
    result[["_source_notes"]][[1]],
    "Overall agreement across all metrics: 50% \\(3/6\\)"
  ))

  result_off <- agreement_metrics(x, analysis_col = "Analysis", show_aggregate_total = FALSE)
  expect_length(result_off[["_source_notes"]], 0)
})

test_that("agreement_metrics show_aggregate adds a per-metric summary row and renders without error", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Perfect", 0.80, 0.70, 0.90, 0.80, 0.70, 0.90,
    "Disagree", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2
  )

  result_off <- agreement_metrics(x, analysis_col = "Analysis")
  expect_length(result_off[["_summary"]], 0)

  result <- agreement_metrics(x, analysis_col = "Analysis", show_aggregate = TRUE)
  expect_length(result[["_summary"]], 1)

  # gt::grand_summary_rows() evaluates its summary functions lazily at
  # render time, so only rendering (not just inspecting `_summary`) can
  # catch a summary function that fails to resolve at that point
  html <- gt::as_raw_html(result)
  expect_true(stringr::str_detect(html, "% Agreement"))
  expect_true(stringr::str_detect(html, "50%"))
})

test_that("agreement_metrics show_aggregate and show_aggregate_total pool only the selected metrics subset", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Perfect", 0.80, 0.70, 0.90, 0.80, 0.70, 0.90,
    "Disagree", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2
  )

  result <- agreement_metrics(
    x,
    analysis_col = "Analysis",
    metrics = c("significance_agreement", "estimate_agreement"),
    show_aggregate = TRUE
  )

  summary_columns <- result[["_summary"]][[1]][["columns"]]
  expect_equal(summary_columns, c("significance_agreement", "estimate_agreement"))

  # pooled across the 2 selected metrics x 2 rows = 2 Yes / 4 total
  expect_true(stringr::str_detect(
    result[["_source_notes"]][[1]],
    "Overall agreement across all metrics: 50% \\(2/4\\)"
  ))

  html <- gt::as_raw_html(result)
  expect_true(stringr::str_detect(html, "50%"))
})

test_that("agreement_metrics aggregate helpers handle an all-NA metric without erroring", {
  # degenerate RCT bounds (lower == upper) make every row's SMD calculation
  # fail, leaving smd_agreement == "NA" for all rows -> zero-row denominator
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Degenerate 1", 0.87, 0.87, 0.87, 0.82, 0.76, 0.87,
    "Degenerate 2", 0.80, 0.80, 0.80, 0.75, 0.65, 0.85
  )

  result <- suppressWarnings(agreement_metrics(
    x,
    analysis_col = "Analysis",
    metrics = "smd_agreement",
    show_aggregate = TRUE
  ))

  expect_true(stringr::str_detect(result[["_source_notes"]][[1]], "NA"))
  # gt::grand_summary_rows() evaluates its summary functions lazily at
  # render time, so rendering (not just inspecting `_summary`) is required
  # to confirm a zero-denominator metric doesn't error when the table is built
  expect_no_error(gt::as_raw_html(result))
})

test_that("agreement_metrics validates show_aggregate and show_aggregate_total", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Main analysis", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87
  )

  expect_error(
    agreement_metrics(x, analysis_col = "Analysis", show_aggregate = "yes"),
    "<show_aggregate> must be a single logical value"
  )
  expect_error(
    agreement_metrics(x, analysis_col = "Analysis", show_aggregate_total = c(TRUE, FALSE)),
    "<show_aggregate_total> must be a single logical value"
  )
})

test_that("agreement_metrics smd_scale defaults to log and reproduces existing SMD values", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Test", 0.3, 0.2, 0.4, 0.5, 0.4, 0.6
  )

  expected_smd <- {
    num <- log(0.3) - log(0.5)
    var_rct <- (log(0.4) - log(0.2)) / (2 * 1.96)
    var_rwe <- (log(0.6) - log(0.4)) / (2 * 1.96)
    num / sqrt(var_rct^2 + var_rwe^2)
  }

  result_default <- agreement_metrics(x, analysis_col = "Analysis", metrics = "smd_agreement")
  result_explicit_log <- agreement_metrics(
    x,
    analysis_col = "Analysis",
    metrics = "smd_agreement",
    smd_scale = "log"
  )

  expect_equal(result_default[["_data"]], result_explicit_log[["_data"]])
  expect_true(stringr::str_detect(
    result_default[["_data"]][["smd_agreement"]],
    sprintf("\\(%s\\)", format(expected_smd, digits = 2, nsmall = 2))
  ))
})

test_that("agreement_metrics smd_scale = 'identity' skips the log transform", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Test", 0.3, 0.2, 0.4, 0.5, 0.4, 0.6
  )

  expected_smd <- {
    num <- 0.3 - 0.5
    var_rct <- (0.4 - 0.2) / (2 * 1.96)
    var_rwe <- (0.6 - 0.4) / (2 * 1.96)
    num / sqrt(var_rct^2 + var_rwe^2)
  }

  result <- agreement_metrics(
    x,
    analysis_col = "Analysis",
    metrics = "smd_agreement",
    smd_scale = "identity"
  )

  expect_true(stringr::str_detect(
    result[["_data"]][["smd_agreement"]],
    sprintf("\\(%s\\)", format(expected_smd, digits = 2, nsmall = 2))
  ))
})

test_that("agreement_metrics smd_scale = 'logit' applies the logit transform", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Test", 0.3, 0.2, 0.4, 0.5, 0.4, 0.6
  )

  logit <- function(p) log(p / (1 - p))
  expected_smd <- {
    num <- logit(0.3) - logit(0.5)
    var_rct <- (logit(0.4) - logit(0.2)) / (2 * 1.96)
    var_rwe <- (logit(0.6) - logit(0.4)) / (2 * 1.96)
    num / sqrt(var_rct^2 + var_rwe^2)
  }

  result <- agreement_metrics(
    x,
    analysis_col = "Analysis",
    metrics = "smd_agreement",
    smd_scale = "logit"
  )

  expect_true(stringr::str_detect(
    result[["_data"]][["smd_agreement"]],
    sprintf("\\(%s\\)", format(expected_smd, digits = 2, nsmall = 2))
  ))
})

test_that("agreement_metrics validates smd_scale", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Main analysis", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87
  )

  expect_error(
    agreement_metrics(x, analysis_col = "Analysis", smd_scale = "bogus"),
    "should be one of"
  )
})

test_that("agreement_metrics footnote text reflects the selected smd_scale", {
  x <- tibble::tribble(
    ~Analysis, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
    "Test", 0.3, 0.2, 0.4, 0.5, 0.4, 0.6
  )

  footnote_for <- function(scale) {
    result <- agreement_metrics(x, analysis_col = "Analysis", smd_scale = scale)
    result[["_footnotes"]][["footnotes"]][[1]]
  }

  expect_true(stringr::str_detect(footnote_for("log"), "based on log-transformed estimates"))
  expect_true(stringr::str_detect(footnote_for("identity"), "based on estimates' original scale"))
  expect_true(stringr::str_detect(footnote_for("logit"), "based on logit-transformed estimates"))
})
