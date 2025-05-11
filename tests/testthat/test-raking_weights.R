test_that("raking_weights returns expected structure", {
  result <- raking_weights(x = mimids, targets = targets_list)

  expect_type(result, "list")
  expect_equal(length(result), 5)
  expect_true(all(sapply(result, is.data.frame)))
  expect_true(all(sapply(result, function(df) "weights" %in% names(df))))
})

test_that("raking_weights errors on non-mimids/wimids input", {
  expect_error(raking_weights(x = list(), targets = targets_list), "needs to be a mimids or wimids object")
})

test_that("raking_weights errors on non-list targets", {
  expect_error(raking_weights(x = mimids, targets = "not a list"), "needs to be a list")
})

test_that("raking_weights errors on unnamed list targets", {
  bad_targets <- c(.5, .5)
  expect_error(raking_weights(x = mimids, targets = bad_targets), "<targets> needs to be a list")
})

test_that("raking_weights errors on non-numeric target vectors", {
  bad_targets <- list(c_smoking_history = c("yes", "no"))
  expect_error(raking_weights(x = mimids, targets = bad_targets), "needs to be a list of named vectors")
})

test_that("raking_weights returns expected structure", {
  result <- raking_weights(x = mimids, targets = targets_list)

  # create survey object
  data_svy <- survey::svydesign(ids = ~ 1, weights = ~ weights, data = result[[1]])

  # print
  weighted_prop <- suppressWarnings(data_svy |>
    gtsummary::tbl_svysummary(
      by = treat,
      include = c_smoking_history
      ) |>
    gtsummary::add_overall())

  string <- "%"
  matches <- regmatches(weighted_prop$table_body$stat_0, regexpr("(\\d+(?:\\.\\d+)?)%", weighted_prop$table_body$stat_0))
  expect_equal(matches, c("65%", "35%"))   # Returns "65%"


})
