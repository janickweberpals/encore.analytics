# encore.analytics 0.3.0

* Added a `...` argument to `km_pooling()` that forwards additional arguments to the per-imputation `ggsurvfit::survfit2()` call (and onwards to `survival::survfit()`), e.g. `type = "fleming-harrington"`. Arguments that `km_pooling()` sets internally (`formula`, `data`, `weights`, `cluster`, `robust`) raise an informative error if supplied this way
* Fixed a shortcoming in `agreement_metrics()` where passing an `estimate_label` other than `"HR (95% CI)"` (e.g. `"OR (95% CI)"` or a custom label) raised an "object 'footnote_label' not found" error; the footnote now includes a matching abbreviation for recognized labels ("HR (95% CI)", "OR (95% CI)", "RR (95% CI)") and simply omits it for unrecognized labels
* Fixed a bug in `agreement_metrics()` where `significance_agreement` could report `"Yes"` even when the RWE confidence interval crossed the null value of 1 (i.e. was not statistically significant), because the underlying check compared the wrong confidence bound. The classification was rewritten to categorize the RCT and RWE results independently by their position relative to the null (entirely above, entirely below, or straddling it) and require both to match; this also fixes a related gap where the "both non-significant" agreement case was only recognized in the protective direction and not in the harmful direction
* Added a `metrics` argument to `agreement_metrics()` that lets callers select which agreement metrics (`significance_agreement`, `estimate_agreement`, `smd_agreement`) to compute and display; unselected metrics — notably the more expensive SMD calculation — are skipped entirely rather than computed and hidden. Default remains all three metrics, preserving existing behavior
* Added `show_aggregate` and `show_aggregate_total` arguments to `agreement_metrics()`. `show_aggregate` (default `FALSE`) adds a summary row at the bottom of the table with the pooled agreement percentage for each displayed metric. `show_aggregate_total` (default `TRUE`) adds a source note below the table with a single agreement percentage pooled across all displayed metrics and rows combined; both exclude "NA" cells (e.g. failed SMD calculations) from the denominator
* Added a `smd_scale` argument to `agreement_metrics()` (`"log"` [default], `"identity"`, or `"logit"`) controlling the transform applied before the SMD calculation, since log-transforming (the previous, hardcoded behavior) is only appropriate for ratio-scale effect measures (HR, OR, RR, IRR). `"identity"` supports difference-scale measures (e.g. risk differences) and `"logit"` supports proportions bounded on \[0, 1\]. Note: `smd_scale` only changes the SMD transform — `agreement_metrics()`'s positivity check and `significance_agreement`'s null value of 1 still assume a ratio-scale effect measure regardless of `smd_scale`, a known limitation for non-ratio-scale effect sizes that is not addressed by this change

# encore.analytics 0.2.1

* `simulate_data()` does not distribute missingness uniformly across all variables anymore, but rather introduces a different (random) missingness on different columns which average to the specified proportion missingness
* Modified `km_pooling()` to handle special situations with time = 0 and survival probability = 1 and survival probability = 0, respectively
* Added argument `time = ` to `km_pooling()` to estimate pooled survival probabilities at specified time points

# encore.analytics 0.2.0

* Initial version of the package
* Linked articles to https://janickweberpals.github.io/imputation-ps-workflows/
* Automated checks using GitHub Actions
