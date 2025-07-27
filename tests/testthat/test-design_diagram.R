# Test suite for design_diagram function
# Load required libraries
library(testthat)
library(tibble)
library(ggplot2)
library(dplyr)

# Test data setup
test_data <- tribble(
  ~variable, ~label, ~encoding, ~dimension, ~measurement_min, ~measurement_max,
  "c_age", "Age [yrs]", "continuous", "Covariate Assessment Window", 0, 0,
  "c_sex", "Sex", "binary", "Covariate Assessment Window", 0, 0,
  "c_ecog", "ECOG Performance Status", "ordinal", "Eligibility Assessment Window", -90, -1,
  "c_comorbidity", "Comorbidity Score", "continuous", "Covariate Assessment Window", -365, -1,
  "c_prior_tx", "Prior Treatment", "binary", "Washout Window", -180, -1,
  "outcome", "Overall Survival", "time-to-event", "Follow-up Period", 0, 365
  )

# Additional test data for edge cases
minimal_data <- tribble(
  ~variable, ~label, ~encoding, ~dimension, ~measurement_min, ~measurement_max,
  "var1", "Variable 1", "continuous", "Dimension A", -30, -10
  )

single_point_data <- tribble(
  ~variable, ~label, ~encoding, ~dimension, ~measurement_min, ~measurement_max,
  "index_var", "Index Variable", "binary", "Index Measurement", 0, 0
  )

# Test 1: Basic functionality
test_that("Basic function execution works", {
  result <- design_diagram(test_data)

  expect_s3_class(result, "ggplot")
  # Remove deprecated is_ggplot() call since expect_s3_class already checks the class
})

# Test 2: Input validation - data frame requirement
test_that("Function requires data frame input", {
  expect_error(
    design_diagram(c(1, 2, 3)),
    "data must be a data frame or tibble"
  )

  expect_error(
    design_diagram(list(a = 1, b = 2)),
    "data must be a data frame or tibble"
  )
})

# Test 3: Input validation - required columns
test_that("Function validates required columns", {
  incomplete_data <- test_data[, 1:4]  # Missing measurement columns

  expect_error(
    design_diagram(incomplete_data),
    "Missing required columns"
  )
})

# Test 4: Input validation - column name types
test_that("Function validates column name parameter types", {
  expect_error(
    design_diagram(test_data, variable_col = 123),
    "Column name parameters must be character strings"
  )

  expect_error(
    design_diagram(test_data, dimension_col = NULL),
    "Column name parameters must be character strings"
  )
})

# Test 5: Input validation - label and time unit parameters
test_that("Function validates label and time unit parameters", {
  expect_error(
    design_diagram(test_data, index_date_label = 123),
    "Label parameters must be character strings"
  )

  expect_error(
    design_diagram(test_data, time_unit = c("Days", "Months")),
    "Label parameters must be character strings"
  )
})

# Test 6: Input validation - box height
test_that("Function validates box height parameter", {
  expect_error(
    design_diagram(test_data, box_height = 0),
    "box_height must be numeric between 0 and 1"
  )

  expect_error(
    design_diagram(test_data, box_height = 1.5),
    "box_height must be numeric between 0 and 1"
  )

  expect_error(
    design_diagram(test_data, box_height = "0.5"),
    "box_height must be numeric between 0 and 1"
  )
})

# Test 7: Input validation - text size
test_that("Function validates text size parameter", {
  expect_error(
    design_diagram(test_data, text_size = 0),
    "text_size must be a positive number"
  )

  expect_error(
    design_diagram(test_data, text_size = -5),
    "text_size must be a positive number"
  )

  expect_error(
    design_diagram(test_data, text_size = "large"),
    "text_size must be a positive number"
  )
})

# Test 8: Custom column names
test_that("Function works with custom column names", {
  custom_data <- test_data
  names(custom_data) <- c("var_name", "var_label", "var_encoding", "dim_name", "min_val", "max_val")

  result <- design_diagram(
    custom_data,
    variable_col = "var_name",
    label_col = "var_label",
    dimension_col = "dim_name",
    min_col = "min_val",
    max_col = "max_val"
  )

  expect_s3_class(result, "ggplot")
})

# Test 9: Default colors functionality
test_that("Default colors work correctly", {
  result <- design_diagram(test_data, colors = NULL)

  expect_s3_class(result, "ggplot")
  # Remove deprecated is.ggplot() call since expect_s3_class already checks the class
})

# Test 10: Unnamed color vector
test_that("Unnamed color vector works correctly", {
  custom_colors <- c("red", "blue", "green", "orange")

  result <- design_diagram(test_data, colors = custom_colors)

  expect_s3_class(result, "ggplot")
})

# Test 11: Named color vector
test_that("Named color vector works correctly", {
  custom_colors <- c(
    "Covariate Assessment Window" = "steelblue",
    "Eligibility Assessment Window" = "darkgreen",
    "Washout Window" = "orange",
    "Follow-up Period" = "purple"
  )

  result <- design_diagram(test_data, colors = custom_colors)

  expect_s3_class(result, "ggplot")
})

# Test 12: Color validation - insufficient unnamed colors
test_that("Function validates sufficient unnamed colors", {
  insufficient_colors <- c("red", "blue")  # Only 2 colors for 4 dimensions

  expect_error(
    design_diagram(test_data, colors = insufficient_colors),
    "Number of colors provided \\(2\\) is less than number of dimensions \\(4\\)"
  )
})

# Test 13: Color validation - missing named colors
test_that("Function validates complete named colors", {
  incomplete_colors <- c(
    "Covariate Assessment Window" = "steelblue",
    "Eligibility Assessment Window" = "darkgreen"
    # Missing "Washout Window" and "Follow-up Period"
  )

  expect_error(
    design_diagram(test_data, colors = incomplete_colors),
    "Colors not provided for dimensions"
  )
})

# Test 14: Color validation - invalid color parameter type
test_that("Function validates color parameter type", {
  expect_error(
    design_diagram(test_data, colors = 123),
    "Invalid colors parameter"
  )

  expect_error(
    design_diagram(test_data, colors = list(a = 1, b = 2)),
    "Invalid colors parameter"
  )
})

# Test 15: Legend functionality
test_that("Legend functionality works correctly", {
  result_with_legend <- design_diagram(test_data, show_variables_legend = TRUE)
  result_without_legend <- design_diagram(test_data, show_variables_legend = FALSE)

  expect_s3_class(result_with_legend, "ggplot")
  expect_s3_class(result_without_legend, "ggplot")

  # Check that legend affects the plot structure
  expect_true(!is.null(result_with_legend$labels$caption))
  expect_true(is.null(result_without_legend$labels$caption))
})

# Test 16: Custom parameters
test_that("Custom parameters work correctly", {
  result <- design_diagram(
    test_data,
    index_date_label = "Custom Index Date",
    time_unit = "Weeks",
    box_height = 0.8,
    text_size = 12
  )

  expect_s3_class(result, "ggplot")
  expect_equal(result$labels$title, "Custom Index Date")
  expect_true(grepl("Weeks", result$labels$x))
})

# Test 17: Minimal data handling
test_that("Function handles minimal data correctly", {
  result <- design_diagram(minimal_data)

  expect_s3_class(result, "ggplot")
})

# Test 18: Single point measurement handling
test_that("Function handles index point measurements", {
  result <- design_diagram(single_point_data)

  expect_s3_class(result, "ggplot")
})

# Test 19: Large dataset handling
test_that("Function handles larger datasets", {
  large_data <- tribble(
    ~variable, ~label, ~encoding, ~dimension, ~measurement_min, ~measurement_max,
    "var1", "Variable 1", "continuous", "Dimension A", -100, -50,
    "var2", "Variable 2", "binary", "Dimension A", -100, -50,
    "var3", "Variable 3", "categorical", "Dimension B", -200, -150,
    "var4", "Variable 4", "continuous", "Dimension B", -200, -150,
    "var5", "Variable 5", "ordinal", "Dimension C", -30, -1,
    "var6", "Variable 6", "binary", "Dimension C", -30, -1,
    "var7", "Variable 7", "continuous", "Dimension D", 0, 365,
    "var8", "Variable 8", "time-to-event", "Dimension D", 0, 365,
    "var9", "Variable 9", "binary", "Dimension E", 0, 0,
    "var10", "Variable 10", "continuous", "Dimension E", 0, 0
  )

  result <- design_diagram(large_data)

  expect_s3_class(result, "ggplot")
})

# Test 20: Edge case - negative time ranges
test_that("Function handles negative time ranges", {
  negative_data <- tribble(
    ~variable, ~label, ~encoding, ~dimension, ~measurement_min, ~measurement_max,
    "var1", "Variable 1", "continuous", "Pre-period", -365, -90,
    "var2", "Variable 2", "binary", "Recent-period", -89, -1
  )

  result <- design_diagram(negative_data)

  expect_s3_class(result, "ggplot")
})

# Test 21: Edge case - positive time ranges
test_that("Function handles positive time ranges", {
  positive_data <- tribble(
    ~variable, ~label, ~encoding, ~dimension, ~measurement_min, ~measurement_max,
    "var1", "Variable 1", "continuous", "Follow-up A", 1, 180,
    "var2", "Variable 2", "binary", "Follow-up B", 181, 365
  )

  result <- design_diagram(positive_data)

  expect_s3_class(result, "ggplot")
})

# Test 22: Mixed time ranges
test_that("Function handles mixed positive and negative time ranges", {
  mixed_data <- tribble(
    ~variable, ~label, ~encoding, ~dimension, ~measurement_min, ~measurement_max,
    "var1", "Variable 1", "continuous", "Pre-period", -180, -90,
    "var2", "Variable 2", "binary", "Index", 0, 0,
    "var3", "Variable 3", "ordinal", "Post-period", 1, 365,
    "var4", "Variable 4", "continuous", "Extended", -30, 90
  )

  result <- design_diagram(mixed_data)

  expect_s3_class(result, "ggplot")
})

# Test 23: Text positioning logic
test_that("Text positioning logic works for different window types", {
  positioning_data <- tribble(
    ~variable, ~label, ~encoding, ~dimension, ~measurement_min, ~measurement_max,
    "var1", "Variable 1", "continuous", "Far Pre-period", -365, -180,  # Should use fit logic
    "var2", "Variable 2", "binary", "Near Pre-period", -90, -1,        # Should place left
    "var3", "Variable 3", "ordinal", "Index Point", 0, 0,              # Index point
    "var4", "Variable 4", "continuous", "Post-period", 1, 365          # Should use fit logic
  )

  result <- design_diagram(positioning_data)

  expect_s3_class(result, "ggplot")
})

# Test 24: Return object structure
test_that("Function returns correct ggplot structure", {
  result <- design_diagram(test_data)

  # Check that it's a ggplot object
  expect_s3_class(result, "ggplot")

  # Check that it has required components
  expect_true(!is.null(result$data))
  expect_true(!is.null(result$layers))
  expect_true(!is.null(result$scales))
  expect_true(!is.null(result$theme))
  expect_true(!is.null(result$labels))

  # Check that labels are set correctly
  expect_true(!is.null(result$labels$title))
  expect_true(!is.null(result$labels$x))
})


