# Changelog

## encore.analytics 0.4.0

### Breaking changes

- **BREAKING CHANGE:**
  [`cox_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/cox_pooling.md)
  no longer applies the `weights` and `subclass` columns of a supplied
  list of data frames automatically, and no longer raises an error if
  there is no `weights` column. Weights and clusters must now be
  specified explicitly, aligned with the new
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
  behavior for lists. Update existing calls such as
  `cox_pooling(x, formula)` to
  `cox_pooling(x, formula, weights = weights)` (weighted data) or
  `cox_pooling(x, formula, weights = weights, cluster = subclass)`
  (matched data). Calls that are not updated now fit an **unweighted**
  model and raise a warning that names the unused `weights`/`subclass`
  columns and the argument to add. Passing `mimids` or `wimids` objects
  is not affected

### New features

- [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
  and
  [`cox_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/cox_pooling.md)
  now accept the same four types of input: weighted (`wimids`) and
  matched (`mimids`) objects, imputed (`mids`) objects (unweighted
  analysis), and lists of data frames, e.g., the output of
  [`raking_weights()`](https://janickweberpals.github.io/encore.analytics/reference/raking_weights.md).
  A new internal helper, `prepare_pooling_input()`, converts each input
  type into a list of datasets plus the default model arguments of that
  type (`weights` for `wimids`; `weights` and `cluster = subclass` for
  `mimids`; none for `mids` and lists), so that both functions share one
  code path
- Arguments passed via `...` are now applied on top of, and take
  precedence over, the default arguments of the input type, e.g.,
  `cluster = NULL` or `weights = w` for weights stored in a column named
  `w`. Only `formula` and `data` are reserved. This relaxes
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md),
  which previously raised an error if `weights`, `cluster` or `robust`
  were supplied via `...`.
  [`cox_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/cox_pooling.md)
  gains a `...` argument that forwards additional arguments to
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html),
  e.g., `ties = "breslow"`
- `robust` is now derived instead of fixed: unless specified,
  `robust = TRUE` is used whenever weights, a cluster or an id are part
  of the model; this reproduces the previous behavior for `mimids`,
  `wimids` objects and weighted lists
- Both functions now print a `cli` message that shows how the datasets
  were obtained (including the `MatchThem::complete(all = FALSE)` call)
  and exactly which model arguments are used and where each comes from
  (default for the input type, derived, or specified by the user). Use
  [`suppressMessages()`](https://rdrr.io/r/base/message.html) to silence
  it
- A warning is raised if the datasets contain a `weights` or `subclass`
  column that is not used because no `weights`/`cluster`/`id` argument
  was specified; specify `weights = NULL` or `cluster = NULL` to confirm
  that the columns should be ignored
- Column names passed as `weights`, `cluster` or `id` are checked
  against the datasets and an informative error is raised if a column
  does not exist
- The documentation of the input types, the default arguments and the
  argument precedence is shared by
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
  and
  [`cox_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/cox_pooling.md)
  and was extended with examples for each input type
- [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
  now handles a single dataset (m = 1, e.g., complete data without
  imputation or a list of length one): since the between-imputation
  variance is 0 by definition, the total variance is set to the
  within-dataset variance and a warning is raised. Previously,
  [`pool.scalar()`](https://amices.org/mice/reference/pool.scalar.html)
  returned `NA` for the total variance, so standard errors, confidence
  intervals and the confidence interval of the median survival time were
  `NA` or misleading without any warning
- Documented and cited the method used by
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
  to pool survival probabilities: Rubin’s rules after a complementary
  log-log transformation of the survival probability,
  `log(-log(1 - S))`, with the within-dataset variance obtained via the
  delta method, as described by Morisot et al. (2015, *BMC Med Res
  Methodol* 15:54, <https://doi.org/10.1186/s12874-015-0048-4>)
  following the recommendation of Marshall et al. (2009, *BMC Med Res
  Methodol* 9:57, <https://doi.org/10.1186/1471-2288-9-57>). The
  calculation itself is unchanged. The documentation previously
  attributed the transformation to a different publication (Marshall,
  Billingham, and Bryan 2009), and has been corrected. The documentation
  now also states that this transformation is applied to `S` itself and
  therefore differs from the `log(-log(S))` used for the confidence
  intervals of `survival::survfit(conf.type = "log-log")`: for a single
  dataset, point estimates and standard errors agree with
  [`survfit()`](https://rdrr.io/pkg/survival/man/survfit.html), but the
  confidence intervals differ slightly
- Added `cli` to `Imports`

## encore.analytics 0.3.0

- Added a `...` argument to
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
  that forwards additional arguments to the per-imputation
  [`ggsurvfit::survfit2()`](http://www.danieldsjoberg.com/ggsurvfit/reference/survfit2.md)
  call (and onwards to
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)),
  e.g. `type = "fleming-harrington"`. Arguments that
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
  sets internally (`formula`, `data`, `weights`, `cluster`, `robust`)
  raise an informative error if supplied this way
- Fixed a shortcoming in
  [`agreement_metrics()`](https://janickweberpals.github.io/encore.analytics/reference/agreement_metrics.md)
  where passing an `estimate_label` other than `"HR (95% CI)"`
  (e.g. `"OR (95% CI)"` or a custom label) raised an “object
  ‘footnote_label’ not found” error; the footnote now includes a
  matching abbreviation for recognized labels (“HR (95% CI)”, “OR (95%
  CI)”, “RR (95% CI)”) and simply omits it for unrecognized labels
- Fixed a bug in
  [`agreement_metrics()`](https://janickweberpals.github.io/encore.analytics/reference/agreement_metrics.md)
  where `significance_agreement` could report `"Yes"` even when the RWE
  confidence interval crossed the null value of 1 (i.e. was not
  statistically significant), because the underlying check compared the
  wrong confidence bound. The classification was rewritten to categorize
  the RCT and RWE results independently by their position relative to
  the null (entirely above, entirely below, or straddling it) and
  require both to match; this also fixes a related gap where the “both
  non-significant” agreement case was only recognized in the protective
  direction and not in the harmful direction
- Added a `metrics` argument to
  [`agreement_metrics()`](https://janickweberpals.github.io/encore.analytics/reference/agreement_metrics.md)
  that lets callers select which agreement metrics
  (`significance_agreement`, `estimate_agreement`, `smd_agreement`) to
  compute and display; unselected metrics — notably the more expensive
  SMD calculation — are skipped entirely rather than computed and
  hidden. Default remains all three metrics, preserving existing
  behavior
- Added `show_aggregate` and `show_aggregate_total` arguments to
  [`agreement_metrics()`](https://janickweberpals.github.io/encore.analytics/reference/agreement_metrics.md).
  `show_aggregate` (default `FALSE`) adds a summary row at the bottom of
  the table with the pooled agreement percentage for each displayed
  metric. `show_aggregate_total` (default `TRUE`) adds a source note
  below the table with a single agreement percentage pooled across all
  displayed metrics and rows combined; both exclude “NA” cells
  (e.g. failed SMD calculations) from the denominator. When
  `show_aggregate_total` is `TRUE`, the pooled percentage is also
  attached directly to the returned gt object
  (e.g. `result$show_aggregate_total`) for programmatic access without
  parsing the source note text
- Added a `smd_scale` argument to
  [`agreement_metrics()`](https://janickweberpals.github.io/encore.analytics/reference/agreement_metrics.md)
  (`"log"` \[default\], `"identity"`, `"logit"`, or `"cloglog"`)
  controlling the transform applied before the SMD calculation, since
  log-transforming (the previous, hardcoded behavior) is only
  appropriate for ratio-scale effect measures (HR, OR, RR, IRR).
  `"identity"` supports difference-scale measures (e.g. risk
  differences), `"logit"` supports proportions bounded on \[0, 1\], and
  `"cloglog"` (the log-log transform of a survival probability,
  `log(-log(p))`) is intended for comparing survival probabilities on
  the scale of
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)’s
  `conf.type = "log-log"` confidence interval method. (Correction: an
  earlier version of this entry stated that this is the scale of the
  Kaplan-Meier estimates pooled by
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md);
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
  pools on `log(-log(1 - p))`, see below.) Note: `smd_scale` only
  changes the SMD transform —
  [`agreement_metrics()`](https://janickweberpals.github.io/encore.analytics/reference/agreement_metrics.md)’s
  positivity check and `significance_agreement`’s null value of 1 still
  assume a ratio-scale effect measure regardless of `smd_scale`, a known
  limitation for non-ratio-scale effect sizes that is not addressed by
  this change
- Added a `digits` argument (default `2`) to
  [`agreement_metrics()`](https://janickweberpals.github.io/encore.analytics/reference/agreement_metrics.md)
  that rounds all RCT/RWE estimates and confidence limits *before* any
  agreement metric is computed, so that e.g. a RWE estimate of 0.799 and
  an RCT confidence limit of 0.80 agree; the table displays the rounded
  values with `digits` decimals. `digits = NULL` disables rounding.
  [`agreement_metrics()`](https://janickweberpals.github.io/encore.analytics/reference/agreement_metrics.md)
  now also validates its inputs more strictly: estimates must be
  positive (checked after rounding), each interval must satisfy
  `lower <= estimate <= upper`, the estimate/limit columns must be
  numeric, and `smd_threshold` must be a single positive number. Missing
  estimates or limits now give an `"NA"` agreement result (excluded from
  the pooled percentages) instead of an error or a misleading `"No"`

## encore.analytics 0.2.1

- [`simulate_data()`](https://janickweberpals.github.io/encore.analytics/reference/simulate_data.md)
  does not distribute missingness uniformly across all variables
  anymore, but rather introduces a different (random) missingness on
  different columns which average to the specified proportion
  missingness
- Modified
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
  to handle special situations with time = 0 and survival probability =
  1 and survival probability = 0, respectively
- Added argument `time =` to
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
  to estimate pooled survival probabilities at specified time points

## encore.analytics 0.2.0

- Initial version of the package
- Linked articles to
  <https://janickweberpals.github.io/imputation-ps-workflows/>
- Automated checks using GitHub Actions
