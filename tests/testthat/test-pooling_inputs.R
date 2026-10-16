test_that("prepare_pooling_input() returns the datasets, default arguments and a note for wimids", {
  out <- prepare_pooling_input(wimids)

  expect_named(out, c("datasets", "args", "note"))
  expect_length(out$datasets, length(wimids$models))
  expect_true(all(vapply(out$datasets, is.data.frame, logical(1))))
  # no observations with a zero weight
  expect_true(all(vapply(out$datasets, function(d) all(d$weights > 0), logical(1))))
  expect_named(out$args, "weights")
  expect_type(out$note, "character")
})

test_that("prepare_pooling_input() adds the matched sets as cluster for mimids", {
  out <- prepare_pooling_input(mimids)

  expect_named(out$args, c("weights", "cluster"))
  expect_true(all(vapply(out$datasets, function(d) "subclass" %in% names(d), logical(1))))
})

test_that("prepare_pooling_input() uses different defaults for wimids, mimids and mids", {
  expect_length(prepare_pooling_input(wimids)$args, 1)
  expect_length(prepare_pooling_input(mimids)$args, 2)
  expect_length(prepare_pooling_input(mids)$args, 0)
})

test_that("prepare_pooling_input() treats mids objects as unweighted", {
  out <- prepare_pooling_input(mids)

  expect_length(out$datasets, mids$m)
  expect_length(out$args, 0)
})

test_that("prepare_pooling_input() uses lists as supplied and applies no arguments", {
  datasets <- MatchThem::complete(mimids, action = "all", all = FALSE, include = FALSE)

  # output of complete(action = "all") (class mild) and plain lists
  for (x in list(datasets, unclass(datasets))) {
    out <- prepare_pooling_input(x)
    expect_equal(unname(out$datasets), unname(unclass(datasets)))
    expect_length(out$args, 0)
  }
})

test_that("prepare_pooling_input() rejects unsupported input", {
  expect_error(prepare_pooling_input(data.frame(x = 1:3)), "list of data frames")
  expect_error(prepare_pooling_input("a"), "list of data frames")
  expect_error(prepare_pooling_input(NULL), "list of data frames")
  expect_error(prepare_pooling_input(list()), "non-empty list of data frames")
  expect_error(prepare_pooling_input(list(1, 2)), "non-empty list of data frames")
})

test_that("fit_models() merges defaults and user arguments, user arguments win", {
  input <- prepare_pooling_input(mimids)

  fit <- suppressMessages(fit_models(
    input = input,
    fit_fun = quote(survival::coxph),
    model_label = "Cox proportional hazards model",
    formula = km_fit,
    user_args = list(cluster = NULL, ties = "breslow")
  ))

  # cluster = NULL dropped the default, ties added, robust derived from the weights
  expect_setequal(names(fit$args), c("weights", "ties", "robust"))
  expect_length(fit$fits, length(mimids$models))
  expect_s3_class(fit$fits[[1]], "coxph")
  # the additional argument reached coxph()
  expect_equal(fit$fits[[1]]$method, "breslow")
})

test_that("fit_models() derives robust only when weights, cluster or id are used", {
  fit_args <- function(input, user_args = list()) {
    suppressMessages(suppressWarnings(fit_models(
      input = input,
      fit_fun = quote(survival::coxph),
      model_label = "Cox proportional hazards model",
      formula = km_fit,
      user_args = user_args
    )))$args
  }

  expect_true(fit_args(prepare_pooling_input(wimids))$robust)
  expect_null(fit_args(prepare_pooling_input(mids))$robust)
  # an explicit robust always wins
  expect_false(fit_args(prepare_pooling_input(wimids), list(robust = FALSE))$robust)
})

test_that("fit_models() tells the user how the models are fitted", {
  input <- prepare_pooling_input(wimids)

  messages <- capture.output(
    invisible(suppressWarnings(fit_models(
      input = input,
      fit_fun = quote(survival::coxph),
      model_label = "Cox proportional hazards model",
      formula = km_fit,
      user_args = list(ties = quote("breslow"))
    ))),
    type = "message"
  )
  messages <- paste(messages, collapse = "\n")

  expect_match(messages, "wimids")
  expect_match(messages, "all = FALSE", fixed = TRUE)
  expect_match(messages, "survival::coxph", fixed = TRUE)
  expect_match(messages, "weights = weights", fixed = TRUE)
  expect_match(messages, "default for the input type", fixed = TRUE)
  expect_match(messages, "ties", fixed = TRUE)
  expect_match(messages, "specified by you", fixed = TRUE)
  expect_match(messages, "derived", fixed = TRUE)
})

test_that("fit_models() reports when a user argument overrides a default", {
  input <- prepare_pooling_input(mimids)

  messages <- paste(capture.output(
    invisible(fit_models(
      input = input,
      fit_fun = quote(survival::coxph),
      model_label = "Cox proportional hazards model",
      formula = km_fit,
      user_args = list(cluster = NULL)
    )),
    type = "message"
  ), collapse = "\n")

  expect_match(messages, "overrides default cluster = subclass", fixed = TRUE)
})

test_that("fit_models() states when no weights or cluster are used", {
  messages <- paste(capture.output(
    invisible(fit_models(
      input = prepare_pooling_input(mids),
      fit_fun = quote(survival::coxph),
      model_label = "Cox proportional hazards model",
      formula = km_fit,
      user_args = list()
    )),
    type = "message"
  ), collapse = "\n")

  expect_match(messages, "no weights, cluster or other arguments used", fixed = TRUE)
})
