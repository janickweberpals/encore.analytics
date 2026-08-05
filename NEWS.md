# encore.analytics 0.3.0

* Fixed a bug in `agreement_metrics()` where passing an `estimate_label` other than `"HR (95% CI)"` (e.g. `"OR (95% CI)"` or a custom label) raised an "object 'footnote_label' not found" error; the footnote now includes a matching abbreviation for recognized labels ("HR (95% CI)", "OR (95% CI)", "RR (95% CI)") and simply omits it for unrecognized labels
* Fixed a bug in `agreement_metrics()` where `significance_agreement` could report `"Yes"` even when the RWE confidence interval crossed the null value of 1 (i.e. was not statistically significant), because the underlying check compared the wrong confidence bound. The classification was rewritten to categorize the RCT and RWE results independently by their position relative to the null (entirely above, entirely below, or straddling it) and require both to match; this also fixes a related gap where the "both non-significant" agreement case was only recognized in the protective direction and not in the harmful direction

# encore.analytics 0.2.1

* `simulate_data()` does not distribute missingness uniformly across all variables anymore, but rather introduces a different (random) missingness on different columns which average to the specified proportion missingness
* Modified `km_pooling()` to handle special situations with time = 0 and survival probability = 1 and survival probability = 0, respectively
* Added argument `time = ` to `km_pooling()` to estimate pooled survival probabilities at specified time points

# encore.analytics 0.2.0

* Initial version of the package
* Linked articles to https://janickweberpals.github.io/imputation-ps-workflows/
* Automated checks using GitHub Actions
