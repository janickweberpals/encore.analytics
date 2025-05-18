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
