# Manually fit and pool Cox proportional hazards model results from multiple imputed datasets

This function manually fits and pools results from Cox proportional
hazards models using a list of imputed datasets. It leverages the
[`mice::as.mira`](https://amices.org/mice/reference/as.mira.html) and
[`mice::pool`](https://amices.org/mice/reference/pool.html) functions to
ensure proper pooling of results across multiple imputations using
Rubin's rules.

## Usage

``` r
cox_pooling(
  x,
  surv_formula = stats::as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat)
)
```

## Arguments

- x:

  A list of imputed datasets with weights or raking weights (e.g., from
  [`raking_weights`](https://janickweberpals.github.io/encore.analytics/reference/raking_weights.md))
  and optional cluster information (for matched datasets).

- surv_formula:

  A formula for the Cox proportional hazards model (default is
  `Surv(fu_itt_months, death_itt) ~ treat`).

## Value

A data frame containing the pooled results, including hazard ratios,
confidence intervals, and p-values.

## Details

The function requires a list of imputed data frames with weights and
optional cluster (matching) information, as well as a formula for the
Cox proportional hazards model. The data frames must include a column
named `weights`, and optionally a column named `subclass` (for matched
datasets to indicate cluster membership).

This function is particularly useful when working with imputed datasets
that are not in the form of `mimids` or `wimids` objects, such as when
intermediate steps like raking weights (via `raking_weights`) are
applied. It provides a flexible way to fit and pool Cox models while
ensuring compatibility with Rubin's rules for multiple imputation.

The function follows these steps:

1.  Fit a Cox proportional hazards model to each imputed dataset. If a
    `subclass` column is present, it is used as a cluster variable for
    matched pairs.

2.  Convert the list of fitted models into a `mira` object using
    [`mice::as.mira`](https://amices.org/mice/reference/as.mira.html).

3.  Pool the results using
    [`mice::pool`](https://amices.org/mice/reference/pool.html), which
    applies Rubin's rules for combining estimates and variances across
    imputations.

4.  Format the pooled results, including exponentiating the hazard
    ratios and calculating confidence intervals.

## See also

[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html),
[`pool`](https://amices.org/mice/reference/pool.html),
[`as.mira`](https://amices.org/mice/reference/as.mira.html),
[`matchthem`](https://rdrr.io/pkg/MatchThem/man/matchthem.html),
[`weightthem`](https://rdrr.io/pkg/MatchThem/man/weightthem.html)

## Examples

``` r
library(encore.analytics)
library(mice)
#> 
#> Attaching package: ‘mice’
#> The following object is masked from ‘package:stats’:
#> 
#>     filter
#> The following objects are masked from ‘package:base’:
#> 
#>     cbind, rbind
library(MatchThem)
#> 
#> Attaching package: ‘MatchThem’
#> The following objects are masked from ‘package:mice’:
#> 
#>     cbind, pool
#> The following object is masked from ‘package:base’:
#> 
#>     cbind

# Simulate a cohort with 500 patients and 20% missing data
data <- simulate_data(
  n = 500,
  imposeNA = TRUE,
  propNA = 0.2
)

# Impute the data
set.seed(42)
mids <- mice(data, m = 5, print = FALSE)
#> Warning: Number of logged events: 765

# Fit a propensity score model
fit <- as.formula(treat ~ dem_age_index_cont + dem_sex_cont + c_smoking_history)

# Weight patients within each imputed dataset
wimids <- weightthem(
  formula = fit,
  datasets = mids,
  approach = "within",
  method = "glm",
  estimand = "ATO"
)
#> Estimating weights     | dataset: #1
#>  #2
#>  #3
#>  #4
#>  #5
#> 

# Create a list of imputed and weighted datasets
wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)

# Define a survival model formula
cox_fit <- as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat)

# Fit and pool Cox proportional hazards model results
cox_pooling(wimids_list, surv_formula = cox_fit)
#>    term  estimate  std.error statistic     p.value  conf.low conf.high
#> 1 treat 0.7297697 0.08792742 -3.582799 0.000374082 0.6139819 0.8673933
#>              b       df dfcom       fmi      lambda m         riv        ubar
#> 1 4.570604e-05 487.4987   496 0.0111428 0.007094245 5 0.007144933 0.007676384
```
