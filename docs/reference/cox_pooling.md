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
  surv_formula = stats::as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat),
  ...
)
```

## Arguments

- x:

  The imputed datasets to analyze. One of:

  - a list of data frames (one per imputed dataset), e.g., the output of
    [`raking_weights`](https://janickweberpals.github.io/encore.analytics/reference/raking_weights.md)
    or of `MatchThem::complete(..., action = "all")`

  - a weighted (`wimids`) or matched (`mimids`) object created with
    MatchThem

  - an imputed (`mids`) object created with mice

  See the section "Input types and default model arguments" for how each
  input is handled.

- surv_formula:

  A formula for the Cox proportional hazards model (default is
  `Surv(fu_itt_months, death_itt) ~ treat`).

- ...:

  Additional arguments passed on to the per-dataset model call
  ([`survfit2`](http://www.danieldsjoberg.com/ggsurvfit/reference/survfit2.md)
  for
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md),
  [`coxph`](https://rdrr.io/pkg/survival/man/coxph.html) for
  `cox_pooling()`). They are used in addition to, and take precedence
  over, the default arguments of the input type (see the section "Input
  types and default model arguments"). Only `formula` and `data` are set
  internally and cannot be supplied here. Column names are written
  without quotes and are looked up in each dataset, e.g. `weights = w`
  or `id = patientid`.

## Value

A data frame containing the pooled results, including hazard ratios,
confidence intervals, and p-values.

## Details

The function takes the imputed datasets (`x`) as a list of data frames,
or as a mimids, wimids or mids object (see the sections "Input types and
default model arguments" and "Precedence of arguments in ..." below), as
well as a formula for the Cox proportional hazards model. Weights and
clusters of lists of data frames are specified explicitly via `...`,
e.g., `weights = weights` for weighted data and
`weights = weights, cluster = subclass` for matched data. A list is
particularly useful when working with imputed datasets that are not in
the form of `mimids` or `wimids` objects, such as when intermediate
steps like raking weights (via `raking_weights`) are applied. Arguments
of the [`coxph`](https://rdrr.io/pkg/survival/man/coxph.html) call can
be supplied via `...`, e.g., `ties = "breslow"`.

**Breaking change in 0.4.0**: previously, a list of data frames had to
contain a column named `weights`, which was used automatically, as was a
column named `subclass` (cluster membership of matched datasets).
Weights and clusters are no longer applied automatically to lists, in
line with
[`km_pooling`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md).
Update calls such as `cox_pooling(x, surv_formula)` to
`cox_pooling(x, surv_formula, weights = weights)` (weighted data) or
`cox_pooling(x, surv_formula, weights = weights, cluster = subclass)`
(matched data). Calls that are not updated fit an unweighted model and
raise a warning that names the unused columns. mimids and wimids objects
are not affected.

The function follows these steps:

1.  Fit a Cox proportional hazards model to each imputed dataset, using
    the weights and cluster (if any) of the input type or specified via
    `...`.

2.  Convert the list of fitted models into a `mira` object using
    [`mice::as.mira`](https://amices.org/mice/reference/as.mira.html).

3.  Pool the results using
    [`mice::pool`](https://amices.org/mice/reference/pool.html), which
    applies Rubin's rules for combining estimates and variances across
    imputations.

4.  Format the pooled results, including exponentiating the hazard
    ratios and calculating confidence intervals.

## Input types and default model arguments

The datasets are first brought into the form of a list of data frames.
Depending on the type of `x`, default model arguments are set:

|  |  |  |  |
|----|----|----|----|
| Input `x` | How the datasets are obtained | `weights` | `cluster` |
| `wimids` | `MatchThem::complete(x, action = "all", all = FALSE, include = FALSE)` | `weights` | none |
| `mimids` | same as `wimids` | `weights` | `subclass` |
| `mids` | `mice::complete(x, action = "all", include = FALSE)` | none (unweighted) | none |
| list of data frames | used as supplied | none | none |

**Lists of data frames are never interpreted automatically.** A list can
come from many sources (weighting, matching, raking, your own code) so
the function cannot know which columns are meant as weights or clusters.
You state this explicitly through `...`, for example:

- weighted data: `weights = weights`

- matched data: `weights = weights, cluster = subclass`

- weights stored in a column with another name: `weights = w`

If the datasets contain a column named `weights` or `subclass` that is
not used because no `weights` or `cluster` (or `id`) argument was
specified, a warning is raised that names the argument to add. Specify
`weights = NULL` or `cluster = NULL` to confirm that the column should
deliberately be ignored.

**Important**: when you build the list from a `mimids` or `wimids`
object yourself, use
`MatchThem::complete(x, action = "all", all = FALSE, include = FALSE)`.
With `all = TRUE`, unmatched patients and patients with zero weight are
included in the models.

## Precedence of arguments in `...`

Default arguments of the input type are applied first, then the
arguments you supply via `...`. If both contain the same argument,
**your argument wins**. For example, for a `mimids` object
`cluster = NULL` fits the model without clustering on the matched sets
and `cluster = pair_id` clusters on another column. `formula` and `data`
are the only reserved arguments.

`robust` is set automatically: unless you specify it, `robust = TRUE` is
used whenever weights, a cluster or an id are part of the model (the
model-based variance is not valid in these cases), otherwise the default
of the model function is kept. An explicitly supplied `robust` always
takes precedence.

## Transparency

Every call prints a message that lists how the datasets were obtained
and exactly which arguments were used for the models, including where
each argument comes from (default for the input type, derived, or
specified by you). Use
[`suppressMessages()`](https://rdrr.io/r/base/message.html) to silence
it.

## See also

[`km_pooling`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md),
[`raking_weights`](https://janickweberpals.github.io/encore.analytics/reference/raking_weights.md),
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

# Define a survival model formula
cox_fit <- as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat)

# 1) Weighted (wimids) or matched (mimids) object:
# weights (and clusters) are set automatically
cox_pooling(wimids, surv_formula = cox_fit)
#> ℹ Input: <wimids>: weighted datasets extracted with MatchThem::complete(action
#>   = "all", all = FALSE, include = FALSE) (m = 5)
#> ℹ Fitting Cox proportional hazards model with survival::coxph() in each
#>   dataset:
#>   formula: survival::Surv(fu_itt_months, death_itt) ~ treat
#>   weights = weights (default for the input type)
#>   robust = TRUE (derived: weights, cluster or id are used)
#>    term  estimate  std.error statistic     p.value  conf.low conf.high
#> 1 treat 0.7297697 0.08792742 -3.582799 0.000374082 0.6139819 0.8673933
#>              b       df dfcom       fmi      lambda m         riv        ubar
#> 1 4.570604e-05 487.4987   496 0.0111428 0.007094245 5 0.007144933 0.007676384

# 2) List of data frames: weights are specified explicitly via `...`
# (all = FALSE excludes observations with zero weight)
wimids_list <- MatchThem::complete(wimids, action = "all", all = FALSE, include = FALSE)
cox_pooling(wimids_list, surv_formula = cox_fit, weights = weights)
#> ℹ Input: list of data frames supplied by the user; no weights or cluster
#>   applied automatically (m = 5)
#> ℹ Fitting Cox proportional hazards model with survival::coxph() in each
#>   dataset:
#>   formula: survival::Surv(fu_itt_months, death_itt) ~ treat
#>   weights = weights (specified by you)
#>   robust = TRUE (derived: weights, cluster or id are used)
#>    term  estimate  std.error statistic     p.value  conf.low conf.high
#> 1 treat 0.7297697 0.08792742 -3.582799 0.000374082 0.6139819 0.8673933
#>              b       df dfcom       fmi      lambda m         riv        ubar
#> 1 4.570604e-05 487.4987   496 0.0111428 0.007094245 5 0.007144933 0.007676384

# for matched datasets, additionally specify the matched sets as clusters:
# cox_pooling(matched_list, surv_formula = cox_fit, weights = weights, cluster = subclass)

# 3) Imputed (mids) object: unweighted Cox model
cox_pooling(mids, surv_formula = cox_fit)
#> ℹ Input: <mids>: imputed datasets extracted with mice::complete(action = "all",
#>   include = FALSE); unweighted (m = 5)
#> ℹ Fitting Cox proportional hazards model with survival::coxph() in each
#>   dataset:
#>   formula: survival::Surv(fu_itt_months, death_itt) ~ treat
#>   no weights, cluster or other arguments used
#>    term  estimate  std.error statistic     p.value  conf.low conf.high b
#> 1 treat 0.7494818 0.09202893 -3.133507 0.001829959 0.6255089 0.8980255 0
#>        df dfcom         fmi lambda m riv        ubar
#> 1 493.962   496 0.004024452      0 5   0 0.008469323

# 4) Pass additional arguments to the underlying coxph() call
cox_pooling(wimids, surv_formula = cox_fit, ties = "breslow")
#> ℹ Input: <wimids>: weighted datasets extracted with MatchThem::complete(action
#>   = "all", all = FALSE, include = FALSE) (m = 5)
#> ℹ Fitting Cox proportional hazards model with survival::coxph() in each
#>   dataset:
#>   formula: survival::Surv(fu_itt_months, death_itt) ~ treat
#>   weights = weights (default for the input type)
#>   ties = "breslow" (specified by you)
#>   robust = TRUE (derived: weights, cluster or id are used)
#>    term  estimate  std.error statistic     p.value  conf.low conf.high
#> 1 treat 0.7297697 0.08792742 -3.582799 0.000374082 0.6139819 0.8673933
#>              b       df dfcom       fmi      lambda m         riv        ubar
#> 1 4.570604e-05 487.4987   496 0.0111428 0.007094245 5 0.007144933 0.007676384
```
