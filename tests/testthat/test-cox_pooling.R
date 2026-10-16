test_that("wimids and cox pooling workflows lead to the same result", {

  library(MatchThem)
  library(survival)
  library(mice)
  suppressWarnings(library(broom))

  # simulate a cohort with 1,000 patients with 20% missing data
  data <- simulate_data(
    n = 500,
    imposeNA = TRUE,
    propNA = 0.2
    )

  # impute the data
  set.seed(42)
  suppressWarnings(mids <- mice(data, m = 5, print = FALSE))

  # fit a propensity score model
  fit <- as.formula(treat ~ dem_age_index_cont + dem_sex_cont + c_smoking_history)

  # weight (or alternatively match) patients within each imputed dataset
  wimids <- weightthem(
    formula = fit,
    datasets = mids,
    approach = "within",
    method = "glm",
    estimand = "ATO"
    )

  # results wimids workflow
  # coxph result
  wimids_workflow_results <- with(
    data = wimids,
    expr = coxph(formula = Surv(fu_itt_months, death_itt) ~ treat,
                 weights = weights,
                 robust = TRUE
                 )
    ) |>
    pool() |>
    tidy(exponentiate = TRUE, conf.int = TRUE)


  # results cox_pooling workflow

  # create a list of imputed and weighted datasets
  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)

  # fit and pool Cox proportional hazards model results
  cox_pooling_workflow_results <- cox_pooling(
    x = wimids_list,
    surv_formula = Surv(fu_itt_months, death_itt) ~ treat,
    weights = weights
    )

  expect_equal(wimids_workflow_results$estimate, cox_pooling_workflow_results$estimate)
  expect_equal(wimids_workflow_results$conf.low, cox_pooling_workflow_results$conf.low)
  expect_equal(wimids_workflow_results$conf.high, cox_pooling_workflow_results$conf.high)

})

test_that("cox_pooling handles invalid inputs appropriately", {
  # Test with non-list input
  expect_error(
    cox_pooling(x = data.frame()),
    "<x> needs to be a mimids, wimids or mids object or a list of data frames"
  )

  # Test with an empty list
  expect_error(
    cox_pooling(x = list()),
    "non-empty list of data frames"
  )

  # Test with invalid formula
  data_with_weights <- list(data.frame(weights = 1))
  expect_error(
    cox_pooling(x = data_with_weights, surv_formula = "not_a_formula"),
    "<surv_formula> needs to be a formula"
  )

  # formula and data are set internally
  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  expect_error(
    cox_pooling(x = wimids_list, surv_formula = km_fit, data = NULL),
    "set internally by cox_pooling"
  )
})

test_that("cox_pooling works with different formula specifications using wimids", {
  # create list of weighted datasets
  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  
  # Test with different formula specifications
  formula1 <- Surv(fu_itt_months, death_itt) ~ treat
  formula2 <- Surv(fu_itt_months, death_itt) ~ treat + dem_age_index_cont
  
  result1 <- cox_pooling(wimids_list, surv_formula = formula1, weights = weights)
  result2 <- cox_pooling(wimids_list, surv_formula = formula2, weights = weights)
  
  # Check structure and content
  expect_s3_class(result1, "data.frame")
  expect_s3_class(result2, "data.frame")
  expect_true(nrow(result2) > nrow(result1))
  expect_true(all(c("estimate", "std.error", "conf.low", "conf.high") %in% names(result1)))
})

test_that("cox_pooling works with matched data from mimids", {
  # Create list of matched datasets
  matched_list <- MatchThem::complete(mimids, action = "all", all = FALSE, include = FALSE)
  
  # Test basic survival model
  result <- cox_pooling(
    x = matched_list,
    surv_formula = km_fit,
    weights = weights,
    cluster = subclass
  )
  
  # Check structure and content
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)  # Only treat coefficient
  expect_true(all(c("estimate", "std.error", "conf.low", "conf.high") %in% names(result)))
})

test_that("cox_pooling results are consistent with manual calculations", {
  matched_list <- MatchThem::complete(mimids, action = "all", all = FALSE, include = FALSE)
  
  # Manual calculation for first imputation
  manual_cox <- survival::coxph(
    formula = km_fit,
    data = matched_list[[1]],
    weights = weights,
    cluster = subclass,
    robust = TRUE
  )
  manual_hr <- exp(coef(manual_cox))
  
  # Cox pooling result
  pooled_result <- cox_pooling(
    x = matched_list,
    surv_formula = km_fit,
    weights = weights,
    cluster = subclass
  )
  
  # The pooled estimate should be in the range of individual estimates
  expect_true(abs(manual_hr - pooled_result$estimate) < 1)
})

test_that("cox_pooling handles interaction terms correctly", {
  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  
  # Formula with interaction
  formula_interaction <- Surv(fu_itt_months, death_itt) ~ treat * dem_age_index_cont
  
  result <- cox_pooling(
    x = wimids_list,
    surv_formula = formula_interaction,
    weights = weights
  )
  
  # Check if interaction term is present
  expect_true(nrow(result) > 1)
  expect_true(any(grepl(":", result$term)))
})

test_that("cox_pooling on wimids/mimids objects equals the equivalent call on a list of data frames", {
  # wimids: weights are set automatically
  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  expect_equal(
    cox_pooling(x = wimids, surv_formula = km_fit),
    cox_pooling(x = wimids_list, surv_formula = km_fit, weights = weights)
  )

  # mimids: weights and the matched sets as cluster are set automatically
  mimids_list <- MatchThem::complete(mimids, action = "all", all = FALSE, include = FALSE)
  expect_equal(
    cox_pooling(x = mimids, surv_formula = km_fit),
    cox_pooling(x = mimids_list, surv_formula = km_fit, weights = weights, cluster = subclass)
  )
})

test_that("cox_pooling on a mids object is unweighted", {
  mids_list <- mice::complete(mids, action = "all", include = FALSE)

  expect_equal(
    cox_pooling(x = mids, surv_formula = km_fit),
    cox_pooling(x = mids_list, surv_formula = km_fit)
  )

  # differs from the weighted result
  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  expect_false(isTRUE(all.equal(
    cox_pooling(x = mids, surv_formula = km_fit)$estimate,
    cox_pooling(x = wimids_list, surv_formula = km_fit, weights = weights)$estimate
  )))
})

test_that("cox_pooling warns if weights and subclass columns are not used", {
  matched_list <- MatchThem::complete(mimids, action = "all", all = FALSE, include = FALSE)

  # BREAKING CHANGE (0.4.0): lists are no longer interpreted automatically
  expect_warning(
    cox_pooling(x = matched_list, surv_formula = km_fit),
    "not used in the models"
  )

  # no warning if weights/cluster are specified, or deliberately set to NULL
  expect_no_warning(cox_pooling(x = matched_list, surv_formula = km_fit, weights = weights, cluster = subclass))
  expect_no_warning(cox_pooling(x = matched_list, surv_formula = km_fit, weights = NULL, cluster = NULL))
})

test_that("cox_pooling accepts weights stored in a column with another name", {
  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  renamed <- lapply(wimids_list, function(d) dplyr::rename(d, w = weights))

  expect_equal(
    cox_pooling(x = renamed, surv_formula = km_fit, weights = w),
    cox_pooling(x = wimids_list, surv_formula = km_fit, weights = weights)
  )

  # a column that does not exist gives an informative error
  expect_error(
    cox_pooling(x = renamed, surv_formula = km_fit, weights = not_a_column),
    "not_a_column"
  )
})

test_that("cox_pooling arguments in ... take precedence over the defaults and are forwarded to coxph()", {
  # cluster = NULL removes the default clustering on the matched sets
  default <- cox_pooling(x = mimids, surv_formula = km_fit)
  no_cluster <- cox_pooling(x = mimids, surv_formula = km_fit, cluster = NULL)
  expect_false(isTRUE(all.equal(default$std.error, no_cluster$std.error)))

  # additional coxph() arguments are accepted
  expect_s3_class(cox_pooling(x = wimids, surv_formula = km_fit, ties = "breslow"), "data.frame")
})
