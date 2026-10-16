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

test_that("km_pooling on a list with weights specified equals the wimids result", {
  datasets <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)

  expect_equal(
    km_pooling(x = datasets, surv_formula = km_fit, weights = weights)$km_survival_table,
    km_pooling(x = wimids, surv_formula = km_fit)$km_survival_table
  )
})

test_that("km_pooling on a list with weights and cluster specified equals the mimids result", {
  datasets <- MatchThem::complete(mimids, action = "all", all = FALSE, include = FALSE)

  expect_equal(
    km_pooling(x = datasets, surv_formula = km_fit, weights = weights, cluster = subclass)$km_survival_table,
    km_pooling(x = mimids, surv_formula = km_fit)$km_survival_table
  )
})

test_that("km_pooling on wimids/mimids objects (legacy) equals the equivalent call on a list of data frames", {

  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  mimids_list <- MatchThem::complete(mimids, action = "all", all = FALSE, include = FALSE)

  compare_results <- function(legacy, list_based) {
    # all returned components: median survival, pooled table and plot data
    expect_named(list_based, names(legacy))
    expect_equal(list_based$km_median_survival, legacy$km_median_survival)
    expect_equal(list_based$km_survival_table, legacy$km_survival_table)
    expect_equal(list_based$km_plot$data, legacy$km_plot$data)
  }

  compare_results(
    legacy = km_pooling(x = wimids, surv_formula = km_fit),
    list_based = km_pooling(x = wimids_list, surv_formula = km_fit, weights = weights)
  )

  compare_results(
    legacy = km_pooling(x = mimids, surv_formula = km_fit),
    list_based = km_pooling(x = mimids_list, surv_formula = km_fit, weights = weights, cluster = subclass)
  )

  # also with time points and additional survfit arguments
  compare_results(
    legacy = km_pooling(x = wimids, surv_formula = km_fit, times = c(6, 12, 24), type = "fleming-harrington"),
    list_based = km_pooling(
      x = wimids_list,
      surv_formula = km_fit,
      times = c(6, 12, 24),
      weights = weights,
      type = "fleming-harrington"
    )
  )
})

test_that("km_pooling on a mids object is unweighted and equals the list of completed datasets", {
  mids_list <- mice::complete(mids, action = "all", include = FALSE)

  expect_equal(
    km_pooling(x = mids, surv_formula = km_fit)$km_survival_table,
    km_pooling(x = mids_list, surv_formula = km_fit)$km_survival_table
  )

  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  expect_false(isTRUE(all.equal(
    km_pooling(x = mids, surv_formula = km_fit)$km_survival_table$surv,
    km_pooling(x = wimids_list, surv_formula = km_fit, weights = weights)$km_survival_table$surv
  )))
})

test_that("km_pooling warns if weights and subclass columns are not used", {
  datasets <- MatchThem::complete(mimids, action = "all", all = FALSE, include = FALSE)

  expect_warning(km_pooling(x = datasets, surv_formula = km_fit), "not used in the models")
  expect_no_warning(km_pooling(x = datasets, surv_formula = km_fit, weights = weights, cluster = subclass))
  expect_no_warning(km_pooling(x = datasets, surv_formula = km_fit, weights = NULL, cluster = NULL))
})

test_that("km_pooling accepts weights stored in a column with another name", {
  wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  renamed <- lapply(wimids_list, function(d) dplyr::rename(d, w = weights))

  expect_equal(
    km_pooling(x = renamed, surv_formula = km_fit, weights = w)$km_survival_table,
    km_pooling(x = wimids, surv_formula = km_fit)$km_survival_table
  )
  expect_error(km_pooling(x = renamed, surv_formula = km_fit, weights = not_a_column), "not_a_column")
})

test_that("km_pooling arguments in ... take precedence over the defaults of the input type", {
  # explicitly passing the defaults changes nothing
  expect_equal(
    km_pooling(x = wimids, surv_formula = km_fit, weights = weights)$km_survival_table,
    km_pooling(x = wimids, surv_formula = km_fit)$km_survival_table
  )

  # weights = NULL removes the default weighting
  unweighted <- km_pooling(x = wimids, surv_formula = km_fit, weights = NULL)$km_survival_table$surv
  weighted <- km_pooling(x = wimids, surv_formula = km_fit)$km_survival_table$surv
  expect_false(isTRUE(all.equal(unweighted, weighted)))
})

test_that("km_pooling accepts the output of raking_weights() when weights are specified", {
  utils::capture.output(raked <- raking_weights(x = mimids, targets = targets_list))

  result <- km_pooling(x = raked, surv_formula = km_fit, weights = weights, cluster = subclass)
  expect_named(result, c("km_median_survival", "km_plot", "km_survival_table"))
  expect_equal(max(result$km_survival_table$m), length(raked))

  # raking weights change the estimate relative to the matched-only result
  expect_false(isTRUE(all.equal(
    result$km_survival_table$surv,
    km_pooling(x = mimids, surv_formula = km_fit)$km_survival_table$surv
  )))
})

test_that("km_pooling validates lists of data frames", {
  datasets <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)

  expect_error(km_pooling(x = list(), surv_formula = km_fit), "non-empty list of data frames")
  expect_error(km_pooling(x = list(1, 2), surv_formula = km_fit), "non-empty list of data frames")
  expect_error(km_pooling(x = mids$data, surv_formula = km_fit), "list of data frames")
})

test_that("km_pooling works with a list containing a single dataset", {
  single <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)[1]

  # the user is warned that there is no between-imputation variance
  expect_warning(
    result <- suppressMessages(km_pooling(x = single, surv_formula = km_fit, weights = weights)),
    "only one dataset"
  )
  tbl <- result$km_survival_table

  # runs and pools over exactly one dataset, output has the same structure
  expect_named(result, c("km_median_survival", "km_plot", "km_survival_table"))
  expect_equal(unique(tbl$m), 1)
  expect_true(all(c("strata", "time", "m", "qbar", "t", "se", "surv", "lower", "upper") %in% names(tbl)))

  # point estimates equal a plain weighted Kaplan-Meier fit of that dataset
  fit <- survival::survfit(km_fit, data = single[[1]], weights = weights)
  first_stratum <- unique(as.character(tbl$strata))[1]
  stratum_tbl <- tbl[tbl$strata == first_stratum & tbl$time > 0, ]
  expect_equal(stratum_tbl$surv[1], fit$surv[1], tolerance = 1e-6)
})

test_that("km_pooling uses the within-dataset variance as total variance for a single dataset", {
  single <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)[1]

  result <- suppressWarnings(suppressMessages(
    km_pooling(x = single, surv_formula = km_fit, weights = weights)
  ))
  tbl <- result$km_survival_table

  # t is not NA and confidence intervals are valid where the survival probability is not 0 or 1
  inner <- tbl[tbl$surv > 0 & tbl$surv < 1, ]
  expect_gt(nrow(inner), 0)
  expect_false(anyNA(inner$t))
  expect_false(anyNA(inner$se))
  expect_true(all(inner$lower <= inner$surv + 1e-8))
  expect_true(all(inner$surv <= inner$upper + 1e-8))
  expect_true(all(inner$upper > inner$lower))

  # the standard error is the within-dataset standard error on the transformed scale
  fit <- ggsurvfit::tidy_survfit(
    ggsurvfit::survfit2(km_fit, data = single[[1]], weights = weights, robust = TRUE),
    type = "survival"
  )
  fit$u <- fit$std.error^2 / ((1 - fit$estimate) * log(1 - fit$estimate))^2
  expect_equal(
    inner$t[1],
    fit$u[fit$strata == inner$strata[1] & fit$time == inner$time[1]],
    tolerance = 1e-6
  )
})

test_that("km_pooling does not warn about a single dataset when several datasets are supplied", {
  datasets <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)

  expect_no_warning(
    suppressMessages(km_pooling(x = datasets, surv_formula = km_fit, weights = weights))
  )
})

test_that("km_pooling for a single dataset reproduces survfit() head to head", {
  single <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)[1]

  tbl <- suppressWarnings(suppressMessages(
    km_pooling(x = single, surv_formula = km_fit, weights = weights)
  ))$km_survival_table

  fit <- survival::survfit(km_fit, data = single[[1]], weights = weights, robust = TRUE)
  ref <- summary(fit, censored = TRUE)
  ref <- data.frame(
    strata = sub("treat=", "", as.character(ref$strata)),
    time = ref$time,
    surv = ref$surv,
    std.err = ref$std.err
  )

  cmp <- merge(transform(tbl, strata = as.character(strata)), ref, by = c("strata", "time"))

  # every observed time point of survfit() is in the pooled table
  # (time 0 is added by tidy_survfit() and has no counterpart in summary())
  expect_equal(nrow(cmp), nrow(ref))
  expect_equal(cmp$surv.x, cmp$surv.y, tolerance = 1e-10)

  # total variance = within-dataset variance on the cloglog scale, from survfit's own std.err
  inner <- cmp[cmp$surv.x > 0 & cmp$surv.x < 1, ]
  expected_t <- inner$std.err^2 / ((1 - inner$surv.x) * log(1 - inner$surv.x))^2
  expect_equal(inner$t, expected_t, tolerance = 1e-8)
})

test_that("km_pooling implements Rubin's rules after complementary log-log transformation (Morisot et al., 2015)", {
  datasets <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  times <- c(6, 12, 24)

  # independent implementation of the formulas of Morisot et al. (2015):
  # per dataset i and time t: Q_i = log(-log(1 - S_i)),
  # U_i = Var[S_i] / [(1 - S_i) * log(1 - S_i)]^2 (delta method)
  per_dataset <- lapply(datasets, function(d) {
    fit <- survival::survfit(km_fit, data = d, weights = weights, robust = TRUE)
    s <- summary(fit, times = times, extend = TRUE)
    data.frame(
      strata = sub("treat=", "", as.character(s$strata)),
      time = s$time,
      S = s$surv,
      var_S = s$std.err^2
    )
  })
  per_dataset <- do.call(rbind, per_dataset)
  per_dataset$Q <- log(-log(1 - per_dataset$S))
  per_dataset$U <- per_dataset$var_S / ((1 - per_dataset$S) * log(1 - per_dataset$S))^2

  m <- length(datasets)
  expected <- do.call(rbind, lapply(
    split(per_dataset, list(per_dataset$strata, per_dataset$time)),
    function(g) {
      q_bar <- mean(g$Q)                                   # Rubin (1987), eq. 1 in Morisot et al.
      u_bar <- mean(g$U)                                   # eq. 3
      b <- sum((g$Q - q_bar)^2) / (m - 1)                  # eq. 4
      total <- u_bar + (1 + 1 / m) * b                     # eq. 2
      data.frame(
        strata = g$strata[1],
        time = g$time[1],
        qbar = q_bar,
        t = total,
        surv = 1 - exp(-exp(q_bar)),                       # eq. 5
        lower = 1 - exp(-exp(q_bar - 1.96 * sqrt(total))), # eq. 6
        upper = 1 - exp(-exp(q_bar + 1.96 * sqrt(total)))
      )
    }
  ))

  result <- suppressMessages(
    km_pooling(x = datasets, surv_formula = km_fit, times = times, weights = weights)
  )$km_survival_table
  result <- as.data.frame(result)
  result$strata <- as.character(result$strata)

  cmp <- merge(result, expected, by = c("strata", "time"), suffixes = c("", ".expected"))
  expect_equal(nrow(cmp), nrow(expected))
  expect_equal(unique(cmp$m), m)
  for (col in c("qbar", "t", "surv", "lower", "upper")) {
    expect_equal(cmp[[col]], cmp[[paste0(col, ".expected")]], tolerance = 1e-8, label = col)
  }
})
