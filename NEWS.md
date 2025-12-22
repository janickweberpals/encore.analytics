# encore.analytics 0.2.1

* `simulate_data()` does not distribute missingness uniformly across all variables anymore, but rather introduces a different (random) missingness on different columns which average to the specified proportion missingness
* Modified `km_pooling()` to handle special situations with time = 0 and survival probability = 1 and survival probability = 0, respectively
* Added argument `time = ` to `km_pooling()` to estimate pooled survival probabilities at specified time points

# encore.analytics 0.2.0

* Initial version of the package
* Linked articles to https://janickweberpals.github.io/imputation-ps-workflows/
* Automated checks using GitHub Actions
