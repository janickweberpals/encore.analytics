# Estimate raking weights for a mimids or wimids object

Function estimates raking weights for multiple imputed and matched
(`mimids`) or weighted (`wimids`) datasets. That is to match the
distributions of certain variables in the imputed and matched/weighted
datasets to a distribution of a target population (e.g., clinical trial
population).

## Usage

``` r
raking_weights(x, targets = NULL)
```

## Arguments

- x:

  imputed and matched (mimids) or weighted (wimids) object

- targets:

  list of all target values for the raking procedure

## Value

list of data frames with updated raking weights (`raking_weights`)

## Details

The function requires an object of class mimids or wimids (`x`), which
is the output of a workflow that requires imputing multiple (m) datasets
using mice or amelia and matching or weighting each imputed dataset via
the MatchThem package (see examples).

The function additionally requires a list of target distributions
(`targets`) for each variable that is considered for the raking
procedure. The list should contain named vectors with the target
distributions for each variable and the names of the vectors should
match the variable names in the imputed datasets.

In brief, the raking procedure iteratively adjusts the weights to make
the weighted sample percentages match the target population percentages
for the selected variables.It does this by multiplying the current
weight for each case by a factor based on the ratio of the target
population proportion to the weighted sample proportion for a given
category. This adjustment is performed sequentially for each category of
each selected variable. Because adjusting for one variable can disrupt
the match for previous variables, the process is repeated through all
selected variables in cycles. This iterative process minimizes the
Kullback-Leibler (KL) divergence and continues until the weighted sample
proportions match the target population proportions for all categories
("full convergence"), or until no further change occurs.

The function follows the following logic:

1.  Extract the ith imputed dataset from the mimids or wimids object

2.  Create a temporary case/patient ID

3.  Apply the
    [`anesrake`](https://rdrr.io/pkg/anesrake/man/anesrake.html)
    function to the ith imputed dataset

4.  Create a temporary dataframe with the case ID and replace the
    initial weights with the updated raking weights

5.  Merge the temporary dataframe with the ith imputed dataset

6.  Drop the temporary case ID

7.  Return the ith imputed dataset with the raking weights

The function returns a list of data frames with the updated raking
weights. These updated raking weights overwrite in each data frame the
existing `weights` column. This column can then be used in a downstream
analysis (e.g., Kaplan-Meier, Cox proportional hazards regression).

## See also

[`anesrake`](https://rdrr.io/pkg/anesrake/man/anesrake.html)
[`matchthem`](https://rdrr.io/pkg/MatchThem/man/matchthem.html)
[`weightthem`](https://rdrr.io/pkg/MatchThem/man/weightthem.html)

## Examples

``` r
 library(encore.analytics)
 library(mice)
 library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
 library(MatchThem)
 library(survival)

 # simulate a cohort with 1,000 patients with 20% missing data
 data <- simulate_data(
   n = 500,
   imposeNA = TRUE,
   propNA = 0.2
   ) |>
   # anesrake works best with factor variables
   mutate(c_smoking_history = factor(ifelse(c_smoking_history == TRUE, "Current/former", "Never")))

 # impute the data (create mids object)
 set.seed(42)
 mids <- mice(data, m = 5, print = FALSE)
#> Warning: Number of logged events: 765

 # define covariates for propensity score model
 covariates <- data |>
  select(starts_with("c_"), starts_with("dem_")) |>
   colnames()

 # define propensity score model
 fit <- as.formula(paste("treat ~", paste(covariates, collapse = " + ")))

 # match patients within each imputed dataset
 mimids <- matchthem(
   formula = fit,
   datasets = mids,
   approach = 'within',
   method = 'nearest'
   )
#> 
#> Matching Observations  | dataset: #1
#> Warning: Fewer control units than treated units; not all treated units will get
#> a match.
#>  #2
#> Warning: Fewer control units than treated units; not all treated units will get
#> a match.
#>  #3
#> Warning: Fewer control units than treated units; not all treated units will get
#> a match.
#>  #4
#> Warning: Fewer control units than treated units; not all treated units will get
#> a match.
#>  #5
#> Warning: Fewer control units than treated units; not all treated units will get
#> a match.
#> 

 smoker_target <- c(.35, .65)
 names(smoker_target) <- c("Current/former", "Never")

 # summarize target distributions in a named list vector
 targets <- list(smoker_target)
 names(targets) <- c("c_smoking_history")

 # estimate raking weights
 mirwds <- raking_weights(
   x = mimids,
   targets = targets
   )
#> [1] "Raking converged in 3 iterations"
#> [1] "Raking converged in 8 iterations"
#> [1] "Raking converged in 3 iterations"
#> [1] "Raking converged in 3 iterations"
#> [1] "Raking converged in 3 iterations"
```
