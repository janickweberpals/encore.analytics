# Calculate Agreement Metrics Between RCT and RWE Results

Creates a formatted table comparing results from randomized controlled
trials (RCTs) with real-world evidence (RWE) emulation studies by
calculating agreement metrics.

## Usage

``` r
agreement_metrics(
  x,
  analysis_col,
  group_col = NULL,
  estimate_label = "HR (95% CI)",
  smd_threshold = 1.96
)
```

## Arguments

- x:

  A data.frame or tibble containing RCT and RWE results (in wide format,
  see examples). Must contain these columns:

  - rct_estimate = Point estimate from the RCT

  - rct_lower = Lower confidence limit of the RCT result

  - rct_upper = Upper confidence limit of the RCT result

  - rwe_estimate = Point estimate from the RWE emulation study

  - rwe_lower = Lower confidence limit of the RWE result

  - rwe_upper = Upper confidence limit of the RWE result

- analysis_col:

  Character. Name of column identifying different analyses

- group_col:

  Character. Optional. Name of column for grouping results (e.g.,
  database)

- estimate_label:

  Character. Label for estimates in table header. Default: "HR (95% CI)"

- smd_threshold:

  Numeric. Threshold for SMD agreement. Default: 1.96 (alpha=0.05)

## Value

A gt table object with formatted agreement metrics

## Details

Calculates three types of agreement metrics:

- Statistical significance agreement: Same direction and statistical
  significance

- Estimate agreement: RWE estimate within RCT confidence interval

- SMD agreement: Standardized mean difference below threshold (default
  1.96)

All estimates should be hazard ratios (HR) and must be positive.
Estimates are log-transformed for SMD calculation.

## Examples

``` r
# Example 1: One analysis per database with grouping

x <- tibble::tribble(
~Analysis, ~Database, ~rct_estimate, ~rct_lower, ~rct_upper, ~rwe_estimate, ~rwe_lower, ~rwe_upper,
"Main analysis", "Database 1", 0.87, 0.78, 0.97, 0.82, 0.76, 0.87,
"Main analysis", "Database 2", 0.5, 0.4, 0.6, 2.0, 1.8, 2.2,
"Main analysis", "Database 3", 0.8, 0.7, 0.9, 1.5, 1.4, 1.6
)

agreement_metrics(x, analysis_col = "Analysis", group_col = "Database")


  


Analysis
```
