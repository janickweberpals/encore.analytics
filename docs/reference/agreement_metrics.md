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
  smd_threshold = 1.96,
  metrics = c("significance_agreement", "estimate_agreement", "smd_agreement"),
  show_aggregate = FALSE,
  show_aggregate_total = TRUE,
  smd_scale = c("log", "identity", "logit", "cloglog"),
  digits = 2
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

  Character. Label for estimates in table header. Default: "HR (95%
  CI)". If the value is one of "HR (95% CI)", "OR (95% CI)", or "RR (95%
  CI)", the table footnote includes a matching abbreviation definition
  (e.g. "HR = Hazard ratio"); any other value is used as-is for the
  header without adding an abbreviation definition to the footnote.

- smd_threshold:

  Numeric. Threshold for SMD agreement; a single positive number.
  Default: 1.96 (alpha=0.05)

- metrics:

  Character vector. Which agreement metrics to compute and display. Must
  be a subset of "significance_agreement", "estimate_agreement",
  "smd_agreement". Default: all three. Metrics excluded here are skipped
  entirely (not computed), which is most relevant for "smd_agreement"
  since it is the most expensive metric to compute and may not be
  meaningful for all effect size types.

- show_aggregate:

  Logical. If TRUE, adds a summary row at the bottom of the table with
  the pooled agreement percentage for each selected metric (Yes / (Yes +
  No) across all rows, excluding "NA" cells, e.g. failed SMD
  calculations, from the denominator). Default: FALSE.

- show_aggregate_total:

  Logical. If TRUE, adds a source note below the table with a single
  agreement percentage pooled across all selected metrics and all rows
  combined, using the same Yes / (Yes + No) logic as `show_aggregate`.
  Default: TRUE. When TRUE, the formatted percentage string (e.g. "78%
  (109/140)") is also attached directly to the returned gt object as
  `show_aggregate_total`, so it can be accessed programmatically (e.g.
  `result$show_aggregate_total`) without parsing it back out of the
  source note text.

- smd_scale:

  Character. Which transform to apply to the RCT/RWE estimates and
  confidence limits before computing the SMD. One of "log" (default),
  "identity", "logit", or "cloglog". "log" is appropriate for
  ratio-scale measures (HR, OR, RR, IRR), since their confidence
  intervals are constructed via the delta method on the log scale.
  "identity" skips the transform, for measures already on an
  additive/difference scale (e.g. risk differences). "logit" is for
  proportions bounded on \[0, 1\]. "cloglog" is the log-log transform of
  a survival probability, `log(-log(p))`, which is the scale of the
  "log-log" confidence interval method used by
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
  (`conf.type = "log-log"`). Note that
  [`km_pooling()`](https://janickweberpals.github.io/encore.analytics/reference/km_pooling.md)
  pools survival probabilities on a different scale, `log(-log(1 - p))`
  (Morisot et al. 2015), see its documentation. Each option expects
  inputs in a particular domain (log: \> 0; logit and cloglog: strictly
  between 0 and 1; identity: any real number); values outside that
  domain will silently produce an "NA" SMD agreement result for that row
  rather than raising an error. "cloglog" is a decreasing function of
  its input, so the lower/upper confidence limits are automatically
  reordered after transforming to preserve the lower-less-than-upper
  invariant required internally. See Details for a related limitation
  that `smd_scale` does not address.

- digits:

  Whole number or NULL. If not NULL, all RCT and RWE point estimates and
  confidence limits are rounded to this many decimal places *before* any
  agreement metric (significance, estimate, SMD) is computed, so that,
  for example, a RWE estimate of 0.799 and an RCT confidence limit of
  0.80 lead to an estimate agreement when `digits = 2`. The estimates
  and limits are displayed with this many decimals (the SMD is always
  shown with 2). Estimates must be positive and each interval must
  satisfy `lower <= estimate <= upper`, checked after rounding. Missing
  values are allowed and give an "NA" agreement result for the affected
  metric. A pre-existing `smd_value` column in `x` is not recomputed or
  rounded. Default: 2. Set to NULL to disable rounding.

## Value

A gt table object with formatted agreement metrics

## Details

Calculates up to three types of agreement metrics, controlled via the
`metrics` argument:

- Statistical significance agreement: RCT and RWE confidence intervals
  fall on the same side of the null value of 1 (both entirely above,
  both entirely below, or both include the null, i.e. both
  non-significant)

- Estimate agreement: RWE estimate within RCT confidence interval

- SMD agreement: Standardized mean difference below threshold (default
  1.96)

Metrics excluded via `metrics` are not computed at all (not just hidden
from the output), which is relevant for the SMD calculation in
particular, since it is the most expensive step and may not be
meaningful for all effect size types.

All estimates should be hazard ratios (HR) and must be positive.
Estimates are transformed before the SMD calculation according to
`smd_scale` (log-transformed by default).

**Known limitation for non-ratio-scale effect sizes**: `smd_scale` only
changes the transform used inside the SMD calculation. It does not
change the function's other ratio-scale assumptions: the positivity
check on `rct_estimate`/`rwe_estimate` and `significance_agreement`'s
comparison against a null value of 1 are applied regardless of
`smd_scale`. For example, setting `smd_scale = "identity"` to use a risk
difference still requires positive input values and still evaluates
statistical significance against a null of 1 rather than 0 — both of
which are incorrect for a difference-scale measure. This is a known,
currently unaddressed limitation; `agreement_metrics()` is designed for
ratio-scale effect measures (HR, OR, RR, IRR) beyond just the SMD
calculation.

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
