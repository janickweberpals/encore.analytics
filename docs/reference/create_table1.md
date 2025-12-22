# Wrapper around `gtsummary::tbl_summary()` to create a beautiful Table 1 quickly

Builds a table 1 using the `gtsummary` package. The function takes a
one-row-per-patient dataframe and creates a summary table of covariates
stratified by treatment group.

## Usage

``` r
create_table1(
  x = NULL,
  covariates = NULL,
  covariates_labels = NULL,
  treat = "treat",
  explicit_na_categorical = TRUE
)
```

## Arguments

- x:

  dataframe with individual-level patient data in a one-row-per-patient
  format with treatment stratification variable and covariates to be
  displayed in the Table 1

- covariates:

  character vector of columns/covariate names to be displayed in Table 1

- covariates_labels:

  named character vector or list of formulas specifying variables labels
  of covariate-label pairs to display in table

- treat:

  character specifying column name of treatment variable

- explicit_na_categorical:

  logical, should missings in categorical variables be explicitly
  included as a separate category (default is TRUE)

## Value

object of class "tbl_summary" "gtsummary"

## Details

Wrapper for
[`tbl_summary`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html).

The function `create_table1` is a wrapper around the
[`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
function. It is designed to create a summary table of covariates
stratified by treatment group. The function requires a
one-row-per-patient dataframe and creates a summary table of covariates
stratified by treatment group.

## Examples

``` r
if (FALSE) { # \dontrun{
library(encore.analytics)

# simulate a cohort with 1,000 patients with 20% missing data
data <- simulate_data(
  n = 1000,
  imposeNA = TRUE,
  propNA = 0.2
  )

# create a Table 1
create_table1(
  x = data,
  covariates = c("dem_age_index_cont", "dem_sex_cont", "c_smoking_history"),
  covariates_labels = list(
    "dem_age_index_cont" = "Age",
    "dem_sex_cont" = "Sex",
    "c_smoking_history" = "Smoking history"
    ),
  treat = "treat"
  )
} # }
```
