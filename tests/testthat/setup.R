library(mice)
library(MatchThem)

# simulate a cohort with 1,000 patients with 20% missing data
data <- simulate_data(
  n = 1000,
  imposeNA = TRUE,
  propNA = 0.2
  )

# impute the data
set.seed(42)
mids <- suppressWarnings(mice(data, m = 5, print = FALSE))

# fit a propensity score model
ps_fit <- as.formula(treat ~ dem_age_index_cont + dem_sex_cont + c_smoking_history)

# weight (or alternatively match) patients within each imputed dataset
wimids <- suppressMessages(weightthem(
  formula = ps_fit,
  datasets = mids,
  approach = "within",
  method = "glm",
  estimand = "ATO"
  ))

mimids <- suppressMessages(suppressWarnings(matchthem(
  formula = ps_fit,
  datasets = mids,
  approach = "within",
  method = "nearest"
  )))

# fit a survival model
km_fit <- as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat)
