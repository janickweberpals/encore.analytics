test_that("km_pooling throws error on incorrect object type", {
  dummy_data <- data.frame(x = 1:10, y = 1:10)
  expect_error(km_pooling(x = dummy_data), "<x> needs to be a mimids or wimids object")
})

test_that("km_pooling throws error on incorrect surv_formula", {
  expect_error(km_pooling(x = wimids, surv_formula = "not_a_formula"), "<surv_formula> needs to be a formula")
})

test_that("km_pooling runs and returns correct structure", {

  result <- km_pooling(x = wimids, surv_formula = km_fit)

  expect_type(result, "list")
  expect_named(result, c("km_median_survival", "km_plot", "km_survival_table"))

  # Check km_median_survival structure
  expect_s3_class(result$km_median_survival, "tbl_df")
  expect_true(all(c("strata", "t_median", "t_lower", "t_upper") %in% names(result$km_median_survival)))

  # Check km_survival_table structure
  expect_s3_class(result$km_survival_table, "tbl_df")
  expect_true(all(c("strata", "time", "m", "qbar", "t", "se", "surv", "lower", "upper") %in% names(result$km_survival_table)))

  # Check that km_plot is a ggplot object
  expect_s3_class(result$km_plot, "gg")
})

test_that("km_pooling works with a matched (mimids) object", {

  result <- km_pooling(x = mimids, surv_formula = km_fit)

  expect_type(result, "list")
  expect_named(result, c("km_median_survival", "km_plot", "km_survival_table"))
  expect_s3_class(result$km_median_survival, "tbl_df")
  expect_s3_class(result$km_survival_table, "tbl_df")
  expect_s3_class(result$km_plot, "gg")
  expect_true(all(
    c("strata", "time", "m", "qbar", "t", "se", "surv", "lower", "upper") %in%
      names(result$km_survival_table)
  ))
})

test_that("km_pooling reports one median-survival row per stratum", {

  result <- km_pooling(x = wimids, surv_formula = km_fit)

  strata <- unique(result$km_survival_table$strata)
  expect_setequal(result$km_median_survival$strata, strata)
  expect_equal(nrow(result$km_median_survival), length(strata))
})

test_that("km_pooling pools across all imputed datasets", {

  result <- km_pooling(x = wimids, surv_formula = km_fit)

  # `m` records the number of imputations Rubin's rule pooled over
  expect_equal(unique(result$km_survival_table$m), length(wimids$models))
})

test_that("km_pooling keeps pooled survival probabilities and CIs within bounds", {

  tbl <- km_pooling(x = wimids, surv_formula = km_fit)$km_survival_table

  expect_true(all(tbl$surv >= 0 & tbl$surv <= 1))

  ci <- tbl[!is.na(tbl$lower) & !is.na(tbl$upper), ]
  expect_true(all(ci$lower <= ci$surv + 1e-8))
  expect_true(all(ci$surv <= ci$upper + 1e-8))
})

test_that("km_pooling throws error on non-numeric times", {
  expect_error(
    km_pooling(x = wimids, surv_formula = km_fit, times = "12"),
    "<times> needs to be a numeric vector"
  )
})

test_that("km_pooling restricts pooling to the requested time points", {

  requested <- c(6, 12, 24)
  result <- km_pooling(x = wimids, surv_formula = km_fit, times = requested)

  expect_true(all(result$km_survival_table$time %in% requested))
  expect_gt(nrow(result$km_survival_table), 0)
})

test_that("km_pooling forwards extra arguments to survfit2()", {

  default <- km_pooling(x = wimids, surv_formula = km_fit)
  fh <- km_pooling(x = wimids, surv_formula = km_fit, type = "fleming-harrington")

  # structure is unchanged
  expect_named(fh, c("km_median_survival", "km_plot", "km_survival_table"))

  # but the underlying estimator changed, so the pooled curve differs
  expect_false(
    isTRUE(all.equal(default$km_survival_table$surv, fh$km_survival_table$surv))
  )
})

test_that("km_pooling rejects arguments it sets internally via ...", {
  for (arg in list(
    list(formula = NULL),
    list(data = NULL),
    list(weights = NULL),
    list(robust = FALSE)
  )) {
    expect_error(
      do.call(
        km_pooling,
        c(list(x = wimids, surv_formula = km_fit), arg)
      ),
      "set internally by km_pooling"
    )
  }
})
