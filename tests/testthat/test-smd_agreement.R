test_that("smd_agreement basic calculations work correctly", {
  # Test case 1: Perfect agreement (SMD should be close to 0)
  smd1 <- smd_agreement(
    rct_estimate = log(0.80),
    rct_lower = log(0.70),
    rct_upper = log(0.90),
    rwe_estimate = log(0.80),
    rwe_lower = log(0.70),
    rwe_upper = log(0.90)
  )
  expect_equal(smd1, 0, tolerance = 1e-10)

  # Test case 2: Known difference
  smd2 <- smd_agreement(
    rct_estimate = log(0.87),
    rct_lower = log(0.78),
    rct_upper = log(0.97),
    rwe_estimate = log(0.82),
    rwe_lower = log(0.76),
    rwe_upper = log(0.87)
  )
  expect_true(abs(smd2) < 1)
})

test_that("smd_agreement handles large differences correctly", {
  smd3 <- smd_agreement(
    rct_estimate = log(0.5),
    rct_lower = log(0.4),
    rct_upper = log(0.6),
    rwe_estimate = log(2.0),
    rwe_lower = log(1.8),
    rwe_upper = log(2.2)
  )
  expect_true(abs(smd3) > 1)
})

test_that("smd_agreement validates inputs correctly", {
  # Test missing arguments
  expect_error(
    smd_agreement(
      rct_estimate = NULL,
      rct_lower = log(0.78),
      rct_upper = log(0.97)
    )
  )

  # Test non-numeric inputs
  expect_error(
    smd_agreement(
      rct_estimate = "invalid",
      rct_lower = log(0.78),
      rct_upper = log(0.97),
      rwe_estimate = log(0.82),
      rwe_lower = log(0.76),
      rwe_upper = log(0.87)
    ),
    "All inputs must be numeric"
  )

  # Test invalid bounds
  expect_error(
    smd_agreement(
      rct_estimate = log(0.80),
      rct_lower = log(0.90),  # Lower > Upper
      rct_upper = log(0.80),
      rwe_estimate = log(0.82),
      rwe_lower = log(0.76),
      rwe_upper = log(0.87)
    ),
    "RCT lower bound must be less than upper bound"
  )
})

test_that("smd_agreement handles edge cases appropriately", {
  # Test with very small differences
  smd4 <- smd_agreement(
    rct_estimate = log(0.80),
    rct_lower = log(0.79),
    rct_upper = log(0.81),
    rwe_estimate = log(0.80),
    rwe_lower = log(0.79),
    rwe_upper = log(0.81)
  )
  expect_equal(smd6, 0, tolerance = 1e-10)

  # Test with identical point estimates but different CIs
  smd5 <- smd_agreement(
    rct_estimate = log(0.80),
    rct_lower = log(0.60),
    rct_upper = log(1.00),
    rwe_estimate = log(0.80),
    rwe_lower = log(0.75),
    rwe_upper = log(0.85)
  )
  expect_equal(smd5, 0, tolerance = 1e-10)

  # Test warning for estimate outside CI
  expect_warning(
    smd_agreement(
      rct_estimate = log(0.5),
      rct_lower = log(0.6),
      rct_upper = log(0.7),
      rwe_estimate = log(0.82),
      rwe_lower = log(0.76),
      rwe_upper = log(0.87)
    ),
    "RCT estimate is outside its confidence interval bounds"
  )
})
