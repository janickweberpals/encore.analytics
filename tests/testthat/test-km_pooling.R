test_that("km_pooling throws error on incorrect object type", {
  dummy_data <- data.frame(x = 1:10, y = 1:10)
  expect_error(km_pooling(x = dummy_data), "<x> needs to be a list of data frames, or a mimids or wimids object")
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
    list(data = NULL)
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

test_that("km_pooling on a list with weights passed via ... equals the wimids result", {

  datasets <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)

  expect_equal(
    km_pooling(x = datasets, surv_formula = km_fit, weights = weights, robust = TRUE)$km_survival_table,
    km_pooling(x = wimids, surv_formula = km_fit)$km_survival_table
  )
})

test_that("km_pooling on a list with weights and cluster passed via ... equals the mimids result", {

  datasets <- MatchThem::complete(mimids, action = "all", all = FALSE, include = FALSE)

  expect_equal(
    km_pooling(
      x = datasets,
      surv_formula = km_fit,
      weights = weights,
      cluster = subclass,
      robust = TRUE
    )$km_survival_table,
    km_pooling(x = mimids, surv_formula = km_fit)$km_survival_table
  )
})

test_that("km_pooling arguments in ... take precedence over mimids/wimids defaults", {

  # explicitly passing the defaults changes nothing
  expect_equal(
    km_pooling(x = wimids, surv_formula = km_fit, weights = weights)$km_survival_table,
    km_pooling(x = wimids, surv_formula = km_fit)$km_survival_table
  )
})

test_that("km_pooling on a list is unweighted unless weights are passed", {

  datasets <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)

  unweighted <- km_pooling(x = datasets, surv_formula = km_fit)$km_survival_table$surv
  weighted <- km_pooling(x = datasets, surv_formula = km_fit, weights = weights)$km_survival_table$surv

  expect_false(isTRUE(all.equal(unweighted, weighted)))
})

test_that("km_pooling rejects a plain mids object", {
  expect_error(km_pooling(x = mids, surv_formula = km_fit), "list of data frames")
})

test_that("km_pooling validates lists of data frames", {

  datasets <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)

  expect_error(km_pooling(x = list(), surv_formula = km_fit), "non-empty list of data frames")
  expect_error(km_pooling(x = list(1, 2), surv_formula = km_fit), "non-empty list of data frames")
  expect_error(
    km_pooling(x = lapply(datasets, function(d) d[, setdiff(names(d), "treat")]), surv_formula = km_fit),
    "not found in all datasets"
  )
  expect_warning(km_pooling(x = datasets[1], surv_formula = km_fit), "only one dataset")
})

test_that("km_pooling accepts the output of raking_weights() and uses the raking weights", {

  utils::capture.output(raked <- raking_weights(x = mimids, targets = targets_list))

  result <- km_pooling(x = raked, surv_formula = km_fit, weights = weights, robust = TRUE)
  expect_named(result, c("km_median_survival", "km_plot", "km_survival_table"))
  expect_equal(max(result$km_survival_table$m), length(raked))

  # raking weights change the estimate relative to the unweighted result
  expect_false(isTRUE(all.equal(
    result$km_survival_table$surv,
    km_pooling(x = raked, surv_formula = km_fit)$km_survival_table$surv
  )))
})

test_that("km_pooling on wimids/mimids objects (legacy) equals the equivalent call on a list of data frames", {

  # legacy behavior: weights/cluster/robust are set automatically
  # list equivalent: same completed datasets, arguments passed explicitly via ...
  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  mimids_list <- MatchThem::complete(mimids, action = "all", all = FALSE, include = FALSE)

  compare_results <- function(legacy, list_based) {
    # all returned components, i.e. median survival, pooled table and plot data
    expect_named(list_based, names(legacy))
    expect_equal(list_based$km_median_survival, legacy$km_median_survival)
    expect_equal(list_based$km_survival_table, legacy$km_survival_table)
    expect_equal(list_based$km_plot$data, legacy$km_plot$data)
  }

  # weighting (wimids)
  compare_results(
    legacy = km_pooling(x = wimids, surv_formula = km_fit),
    list_based = km_pooling(x = wimids_list, surv_formula = km_fit, weights = weights, robust = TRUE)
  )

  # matching (mimids)
  compare_results(
    legacy = km_pooling(x = mimids, surv_formula = km_fit),
    list_based = km_pooling(
      x = mimids_list,
      surv_formula = km_fit,
      weights = weights,
      cluster = subclass,
      robust = TRUE
    )
  )

  # also holds when time points and additional survfit arguments are specified
  compare_results(
    legacy = km_pooling(x = wimids, surv_formula = km_fit, times = c(6, 12, 24), type = "fleming-harrington"),
    list_based = km_pooling(
      x = wimids_list,
      surv_formula = km_fit,
      times = c(6, 12, 24),
      weights = weights,
      robust = TRUE,
      type = "fleming-harrington"
    )
  )
})
