test_that("as_dataset_list() converts a wimids object and sets weights defaults", {
  out <- as_dataset_list(wimids)

  expect_length(out, length(wimids$models))
  expect_true(all(vapply(out, is.data.frame, logical(1))))
  expect_true(all(vapply(out, function(d) all(d$weights > 0), logical(1))))

  defaults <- attr(out, "survfit_defaults")
  expect_named(defaults, c("weights", "robust"))
})

test_that("as_dataset_list() converts a mimids object and adds the cluster default", {
  out <- as_dataset_list(mimids)

  expect_length(out, length(mimids$models))
  expect_true(all(vapply(out, function(d) "subclass" %in% names(d), logical(1))))
  expect_named(attr(out, "survfit_defaults"), c("weights", "robust", "cluster"))
})

test_that("as_dataset_list() returns lists unchanged", {
  datasets <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
  lst <- unclass(datasets)

  expect_identical(as_dataset_list(lst), lst)
  expect_null(attr(as_dataset_list(lst), "survfit_defaults"))
})

test_that("as_dataset_list() rejects unsupported input", {
  expect_error(as_dataset_list(mids), "list of data frames")
  expect_error(as_dataset_list(data.frame(x = 1:3)), "list of data frames")
  expect_error(as_dataset_list("a"), "list of data frames")
})
