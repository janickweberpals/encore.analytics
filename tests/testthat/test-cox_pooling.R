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
