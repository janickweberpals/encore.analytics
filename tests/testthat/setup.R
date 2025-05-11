library(mice)
library(MatchThem)
library(dplyr)

# simulate a cohort with 1,000 patients with 20% missing data
data <- simulate_data(
  n = 1000,
  imposeNA = TRUE,
  propNA = 0.2
  ) |>
  # anesrake works best with factor variables
  mutate(c_smoking_history = factor(ifelse(c_smoking_history == TRUE, "Current/former", "Never")))

# impute the data
set.seed(42)
mids <- suppressWarnings(mice(data, m = 5, print = FALSE))

# fit a propensity score model
covariates <- data |>
select(starts_with("c_"), starts_with("dem_")) |>
 colnames()

# define propensity score model
ps_fit <- as.formula(paste("treat ~", paste(covariates, collapse = " + ")))

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
  method = "nearest",
  ratio = 1,
  distance = "glm",
  link = "logit",
  caliper = 0.05,
  replace = F
  )))

# fit a survival model
km_fit <- as.formula(survival::Surv(fu_itt_months, death_itt) ~ treat)

# Define target for raking
smoker_target <- c(.65, .35)
names(smoker_target) <- c("Current/former", "Never")
targets_list <- list(c_smoking_history = smoker_target)
