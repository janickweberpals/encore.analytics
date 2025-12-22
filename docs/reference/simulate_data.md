# Simulates an artifical EHR-derived analysis-ready oncology dataset

Parameterized function to quickly create an EHR-derived analytic cohort
for analytic code development.

## Usage

``` r
simulate_data(
  n_total = 3500,
  seed = 42,
  include_id = TRUE,
  imposeNA = TRUE,
  propNA = NULL
)
```

## Arguments

- n_total:

  integer, number of total patients

- seed:

  integer, seed for reproducibility

- include_id:

  logical, include a generated patientid variable

- imposeNA:

  logical, set covariates to missing

- propNA:

  numeric, proportion of missingness, needs to be between 0 and 1

## Value

data frame with simulated analytic cohort

## Details

This function simulates a cohort of patients with oncology data. The
cohort is simulated using a Weibull distribution for time to event and a
logistic distribution for treatment assignment. The function also allows
for missingness to be imposed on the data. The function is parameterized
to allow for the number of patients to be simulated, the seed for
reproducibility, and whether to include a patient id variable.

## Examples

``` r
if (FALSE) { # \dontrun{
library(encore.analytics)

# Original uniform missingness
data_uniform <- simulate_data(
  n_total = 3500,
  seed = 41,
  include_id = FALSE,
  imposeNA = TRUE,
  propNA = .33
  )

# Now creates variable missingness across columns with average of 0.33
data_variable <- simulate_data(
  n_total = 3500,
  seed = 41,
  include_id = FALSE,
  imposeNA = TRUE,
  propNA = .33
  )

head(data_variable)

} # }
```
