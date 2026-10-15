test_that("km_pooling throws error on incorrect object type", {
  dummy_data <- data.frame(x = 1:10, y = 1:10)
  expect_error(km_pooling(x = dummy_data), "<x> needs to be a mimids, wimids or mids object or a list of data frames")
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

test_that("km_pooling forwards ... arguments that must be resolved as columns of the data", {

  # `id` (like `weights`/`cluster`) is not an object in this environment --
  # it must be resolved by survfit() as a column of each imputed dataset.
  # This is a regression test: forwarding `...` naively (either via list(...)
  # in the reserved-argument guard, or by splicing a literal `...` into the
  # survfit2() call) breaks this, either by forcing `patientid` too early
  # ("object 'patientid' not found") or by corrupting it into an unresolvable
  # `..1` placeholder inside survfit2()'s internal call reconstruction.
  result <- km_pooling(x = wimids, surv_formula = km_fit, id = patientid)

  expect_named(result, c("km_median_survival", "km_plot", "km_survival_table"))
  expect_gt(nrow(result$km_survival_table), 0)
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

test_that("km_pooling on a list of completed datasets equals the wimids result", {

  datasets <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)

  expect_equal(
    km_pooling(x = datasets, surv_formula = km_fit)$km_survival_table,
    km_pooling(x = wimids, surv_formula = km_fit)$km_survival_table
  )
})

test_that("km_pooling on a list of completed datasets equals the mimids result", {

  # subclass is picked up as cluster variable
  datasets <- MatchThem::complete(mimids, action = "all", all = FALSE, include = FALSE)

  expect_equal(
    km_pooling(x = datasets, surv_formula = km_fit)$km_survival_table,
    km_pooling(x = mimids, surv_formula = km_fit)$km_survival_table
  )
})

test_that("km_pooling works with a mids object and equals the list of completed datasets", {

  mids_obj <- mids

  result <- km_pooling(x = mids_obj, surv_formula = km_fit)

  expect_named(result, c("km_median_survival", "km_plot", "km_survival_table"))
  expect_equal(unique(result$km_survival_table$m), mids_obj$m)
  expect_equal(
    result$km_survival_table,
    km_pooling(
      x = mice::complete(mids_obj, action = "all", include = FALSE),
      surv_formula = km_fit
    )$km_survival_table
  )
})

test_that("km_pooling uses weights of a list only if present", {

  datasets <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  unweighted <- lapply(datasets, function(d) d[, setdiff(names(d), "weights")])

  expect_false(isTRUE(all.equal(
    km_pooling(x = datasets, surv_formula = km_fit)$km_survival_table$surv,
    km_pooling(x = unweighted, surv_formula = km_fit)$km_survival_table$surv
  )))
})

test_that("km_pooling validates lists of data frames", {

  datasets <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)

  expect_error(km_pooling(x = list(), surv_formula = km_fit), "non-empty list of data frames")
  expect_error(km_pooling(x = list(1, 2), surv_formula = km_fit), "non-empty list of data frames")
  expect_error(
    km_pooling(x = lapply(datasets, function(d) d[, setdiff(names(d), "treat")]), surv_formula = km_fit),
    "not found in all datasets"
  )
  mixed <- datasets
  mixed[[1]]$weights <- NULL
  expect_error(km_pooling(x = mixed, surv_formula = km_fit), "all or none")
  expect_warning(km_pooling(x = datasets[1], surv_formula = km_fit), "only one dataset")
})

test_that("km_pooling accepts the output of raking_weights() and uses the raking weights", {

  utils::capture.output(raked <- raking_weights(x = mimids, targets = targets_list))

  result <- km_pooling(x = raked, surv_formula = km_fit)
  expect_named(result, c("km_median_survival", "km_plot", "km_survival_table"))
  expect_equal(max(result$km_survival_table$m), length(raked))

  # raking weights change the estimate relative to the matched-only result
  expect_false(isTRUE(all.equal(
    result$km_survival_table$surv,
    km_pooling(x = mimids, surv_formula = km_fit)$km_survival_table$surv
  )))
})
