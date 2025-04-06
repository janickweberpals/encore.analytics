# tests for simulate_data.R

test_that("simulate_data returns a data frame with expected dimensions", {
  df <- simulate_data(n_total = 100, seed = 123, include_id = TRUE, imposeNA = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 100)})

test_that("simulate_data excludes patientid if include_id = FALSE", {
  df <- simulate_data(n_total = 100, seed = 123, include_id = FALSE, imposeNA = FALSE)
  expect_false("patientid" %in% names(df))
})

test_that("simulate_data includes patientid if include_id = TRUE", {
  df <- simulate_data(n_total = 100, seed = 123, include_id = TRUE, imposeNA = FALSE)
  expect_true("patientid" %in% names(df))
})

test_that("simulate_data imposes missing values when imposeNA = TRUE", {
  df <- simulate_data(n_total = 100, seed = 123, include_id = TRUE, imposeNA = TRUE, propNA = 0.25)
  na_count <- sum(is.na(df))
  expect_gt(na_count, 0)
})

test_that("simulate_data errors when propNA is set but imposeNA is FALSE", {
  expect_error(simulate_data(n_total = 100, imposeNA = FALSE, propNA = 0.2),
               "<propNA> is specified but imposeNA is FALSE")
})

test_that("simulate_data errors with invalid propNA value", {
  expect_error(simulate_data(n_total = 100, imposeNA = TRUE, propNA = 1.5),
               "<propNA> needs to be a numeric between 0 and 1")
})

test_that("simulate_data produces consistent results with same seed", {
  df1 <- simulate_data(n_total = 100, seed = 99, imposeNA = FALSE)
  df2 <- simulate_data(n_total = 100, seed = 99, imposeNA = FALSE)
  expect_equal(df1, df2)
})

test_that("simulate_data has expected columns", {
  df <- simulate_data(n_total = 50, seed = 42, imposeNA = FALSE)
  expected_cols <- c("treat", "dem_age_index_cont", "dem_sex_cont", "dem_race", "fu_itt_months", "death_itt")
  expect_true(all(expected_cols %in% names(df)))
})

test_that("simulate_data handles large n_total without error", {
  df <- simulate_data(n_total = 10000, seed = 123, imposeNA = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 10000)
})

test_that("simulate_data handles small n_total with error", {
  expect_error(simulate_data(n_total = 5, seed = 123, imposeNA = FALSE),
               "<n_total> needs to be larger than 10 for meaningful output")
})

test_that("simulate_data handles small n_total without error", {
  df <- simulate_data(n_total = 11, seed = 123, imposeNA = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 11)
})

test_that("simulate_data handles large n_total with imposed NAs", {
  df <- simulate_data(n_total = 10000, seed = 123, imposeNA = TRUE, propNA = 0.1)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 10000)
})

test_that("simulate_data handles small n_total with imposed NAs", {
  df <- simulate_data(n_total = 11, seed = 123, imposeNA = TRUE, propNA = 0.1)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 11)
})

test_that("simulate_data handles large n_total with imposed NAs and different propNA", {
  df <- simulate_data(n_total = 10000, seed = 123, imposeNA = TRUE, propNA = 0.2)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 10000)
})

test_that("simulate_data handles small n_total with imposed NAs and different propNA", {
  df <- simulate_data(n_total = 11, seed = 123, imposeNA = TRUE, propNA = 0.2)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 11)
})

test_that("simulate_data handles large n_total with imposed NAs and different propNA", {
  df <- simulate_data(n_total = 10000, seed = 123, imposeNA = TRUE, propNA = 0.3)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 10000)
})
