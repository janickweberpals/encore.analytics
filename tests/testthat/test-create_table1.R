# tests for create_table1 function

test_that("create_table1 returns gtsummary object", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("forcats")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("cardx")
  skip_if_not_installed("smd")

  # Simulated data
  data <- simulate_data(
    n = 1000,
    imposeNA = TRUE,
    propNA = 0.2
    )

  result <- create_table1(
    x = data,
    covariates = c("dem_age_index_cont", "dem_sex_cont", "c_smoking_history"),
    covariates_labels = list("dem_age_index_cont" = "Age", "dem_sex_cont" = "Sex", "c_smoking_history" = "Smoking history"),
    treat = "treat"
    )

  expect_s3_class(result, c("tbl_summary", "gtsummary"))
})

test_that("create_table1 errors with wrong input types", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("forcats")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("cardx")
  skip_if_not_installed("smd")

  data <- tibble::tibble(
    treat = sample(c("A", "B"), 10, replace = TRUE),
    age = rnorm(10)
  )

  # wrong data type
  expect_error(create_table1(x = "not_a_df", covariates = "age", treat = "treat"))

  # covariate not in data
  expect_error(create_table1(x = data, covariates = "not_a_column", treat = "treat"))

  # treat not in data
  expect_error(create_table1(x = data, covariates = "age", treat = "not_treat"))

  # invalid covariates_labels
  expect_error(create_table1(x = data, covariates = "age", treat = "treat", covariates_labels = "not_list"))

  # explicit_na_categorical not logical
  expect_error(create_table1(x = data, covariates = "age", treat = "treat", explicit_na_categorical = "yes"))
})

test_that("Missing values are handled explicitly if requested", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("forcats")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("cardx")
  skip_if_not_installed("smd")

  data <- tibble::tibble(
    treat = rep(c("A", "B"), each = 5),
    sex = c("Male", NA, "Female", "Female", "Male", NA, "Male", "Female", NA, "Male")
  )

  result <- create_table1(
    x = data,
    covariates = "sex",
    treat = "treat",
    explicit_na_categorical = TRUE
  )

  # Expect the "Missing" level to be in the result
  expect_true(any(grepl("Missing", result$table_body$label, ignore.case = TRUE)))
})

test_that("Labels are applied correctly", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("forcats")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("cardx")
  skip_if_not_installed("smd")

  data <- tibble::tibble(
    treat = sample(c("A", "B"), 20, replace = TRUE),
    age = rnorm(20),
    sex = sample(c("Male", "Female"), 20, replace = TRUE)
  )

  result <- create_table1(
    x = data,
    covariates = c("age", "sex"),
    treat = "treat",
    covariates_labels = list(age ~ "Age (years)", sex ~ "Biological Sex")
  )

  labels_applied <- result$table_body$label[1:2]
  expect_true(all(c("Age (years)", "Biological Sex") %in% labels_applied))
})
