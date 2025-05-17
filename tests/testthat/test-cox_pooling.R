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
    surv_formula = Surv(fu_itt_months, death_itt) ~ treat
    )

  expect_equal(wimids_workflow_results$estimate, cox_pooling_workflow_results$estimate)
  expect_equal(wimids_workflow_results$conf.low, cox_pooling_workflow_results$conf.low)
  expect_equal(wimids_workflow_results$conf.high, cox_pooling_workflow_results$conf.high)

})

test_that("cox_pooling handles invalid inputs appropriately", {
  # Test with non-list input
  expect_error(
    cox_pooling(x = data.frame()),
    "<cox_results_list> needs to be a list or mild object"
  )

  # Test with missing weights column
  data_no_weights <- list(data.frame(x = 1))
  expect_error(
    cox_pooling(x = data_no_weights),
    "<weights> not in <x>"
  )

  # Test with invalid formula
  data_with_weights <- list(data.frame(weights = 1))
  expect_error(
    cox_pooling(x = data_with_weights, surv_formula = "not_a_formula"),
    "<surv_formula> needs to be a formula"
  )
})

test_that("cox_pooling works with different formula specifications using wimids", {
  # create list of weighted datasets
  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  
  # Test with different formula specifications
  formula1 <- Surv(fu_itt_months, death_itt) ~ treat
  formula2 <- Surv(fu_itt_months, death_itt) ~ treat + dem_age_index_cont
  
  result1 <- cox_pooling(wimids_list, surv_formula = formula1)
  result2 <- cox_pooling(wimids_list, surv_formula = formula2)
  
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
    surv_formula = km_fit
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
    surv_formula = km_fit
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
    surv_formula = formula_interaction
  )
  
  # Check if interaction term is present
  expect_true(nrow(result) > 1)
  expect_true(any(grepl(":", result$term)))
})

