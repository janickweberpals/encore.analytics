#' Estimate raking weights for a mimids or wimids object
#'
#' @description
#' Function estimates raking weights for multiple imputed and matched (\code{mimids})
#' or weighted (\code{wimids}) datasets. That is to match the distributions of
#' certain variables in the imputed and matched/weighted datasets to a distribution of
#' a target population (e.g., clinical trial population).
#'
#' @details
#' The function requires an object of class mimids or wimids (\code{x}), which is the output
#' of a workflow that requires imputing multiple (m) datasets using mice or amelia
#' and matching or weighting each imputed dataset via the MatchThem package
#' (see examples).
#'
#' The function additionally requires a list of target distributions (\code{targets}) for each variable
#' that is considered for the raking procedure. The list should contain named vectors with the target
#' distributions for each variable and the names of the vectors should match the variable names in the imputed datasets.
#'
#' In brief, the raking procedure iteratively adjusts the weights to make the weighted sample percentages match the target
#' population percentages for the selected variables.It does this by multiplying the current weight for each case by a
#' factor based on the ratio of the target population proportion to the weighted sample proportion for a given category.
#' This adjustment is performed sequentially for each category of each selected variable. Because adjusting for one
#' variable can disrupt the match for previous variables, the process is repeated through all selected variables in cycles.
#' This iterative process minimizes the Kullback-Leibler (KL) divergence and continues until the weighted sample proportions
#' match the target population proportions for all categories ("full convergence"), or until no further change occurs.
#'
#' The function follows the following logic:
#' 1. Extract the ith imputed dataset from the mimids or wimids object
#' 2. Create a temporary case/patient ID
#' 3. Apply the \code{\link[anesrake]{anesrake}} function to the ith imputed dataset
#' 4. Create a temporary dataframe with the case ID and replace the initial weights with the updated raking weights
#' 5. Merge the temporary dataframe with the ith imputed dataset
#' 6. Drop the temporary case ID
#' 7. Return the ith imputed dataset with the raking weights
#'
#' The function returns a list of data frames with the updated raking weights. These
#' updated raking weights overwrite in each data frame the existing \code{weights} column.
#' This column can then be used in a downstream analysis (e.g., Kaplan-Meier, Cox proportional hazards regression).
#'
#' @seealso
#' \code{\link[anesrake]{anesrake}} \code{\link[MatchThem]{matchthem}} \code{\link[MatchThem]{weightthem}}
#'
#' @param x imputed and matched (mimids) or weighted (wimids) object
#' @param targets list of all target values for the raking procedure
#'
#' @return list of data frames with updated raking weights (\code{raking_weights})
#'
#' @md
#'
#' @export
#'
#' @examples
#'
#'  library(encore.analytics)
#'  library(mice)
#'  library(dplyr)
#'  library(MatchThem)
#'  library(survival)
#'
#'  # simulate a cohort with 1,000 patients with 20% missing data
#'  data <- simulate_data(
#'    n = 500,
#'    imposeNA = TRUE,
#'    propNA = 0.2
#'    ) |>
#'    # anesrake works best with factor variables
#'    mutate(c_smoking_history = factor(ifelse(c_smoking_history == TRUE, "Current/former", "Never")))
#'
#'  # impute the data (create mids object)
#'  set.seed(42)
#'  mids <- mice(data, m = 5, print = FALSE)
#'
#'  # define covariates for propensity score model
#'  covariates <- data |>
#'   select(starts_with("c_"), starts_with("dem_")) |>
#'    colnames()
#'
#'  # define propensity score model
#'  fit <- as.formula(paste("treat ~", paste(covariates, collapse = " + ")))
#'
#'  # match patients within each imputed dataset
#'  mimids <- matchthem(
#'    formula = fit,
#'    datasets = mids,
#'    approach = 'within',
#'    method = 'nearest'
#'    )
#'
#'  smoker_target <- c(.35, .65)
#'  names(smoker_target) <- c("Current/former", "Never")
#'
#'  # summarize target distributions in a named list vector
#'  targets <- list(smoker_target)
#'  names(targets) <- c("c_smoking_history")
#'
#'  # estimate raking weights
#'  mirwds <- raking_weights(
#'    x = mimids,
#'    targets = targets
#'    )
#'
raking_weights <- function(x,
                           targets = NULL
                           ){

  # input checks----

  # check if object is a mimids or wimids object
  assertthat::assert_that(inherits(x, c("mimids", "wimids")), msg = "<object> needs to be a mimids or wimids object")
  # check if targets is a list
  assertthat::assert_that(is.list(targets), msg = "<targets> needs to be a list")
  # check if targets is a named list
  assertthat::assert_that(all(names(targets) != ""), msg = "<targets> needs to be a named list")
  # check if targets is a list of named vectors
  assertthat::assert_that(all(sapply(targets, is.numeric)), msg = "<targets> needs to be a list of named vectors")
  # check if targets is a list of named vectors
  assertthat::assert_that(all(sapply(targets, function(x) all(names(x) != ""))), msg = "<targets> needs to be a list of named vectors")

  # extract imputed and matched/weighted datasets and weights ----

  # create a function to indicate which cases should be re-weighted
  # since we are only interested in matched/weighted cases
  estimate_raking_weights <- function(i){

    # next, we create a list of datasets to apply the raking procedure one-by-one
    # action = "all"  : produces a mild object (list) of the multiply imputed datasets
    # all = FALSE     : do NOT include observations with a zero estimated weight
    # include = FALSE : do NOT include original data with the missing values

    # Important: the all = parameter needs to be set to FALSE
    # since otherwise unmatched patients or those zero weight
    # will be included, too!
    data <- MatchThem::complete(x, action = i, all = FALSE, include = FALSE)

    # create a temporary case ID
    data <- data |>
      dplyr::mutate(caseid = seq(1, nrow(data), 1))

    # apply anesrake function to re-weight -----------------------------------
    anesrake_out <- anesrake::anesrake(
      # target distributions for raking procedure
      inputter = targets,
      # data to be weighted
      dataframe = data,
      # patient identifier
      caseid = data$caseid,
      # other weights that should be accounted for before re-weighting is conducted
      # for matching this is 0 (unmatched) and 1 (matched) and for weighting this is the actual weights
      weightvec = data$weights,
      # no verbosity
      verbose = FALSE
      )

    # create a temporary dataframe with id and sampling/re-weighting weights
    caseweights <- data.frame(caseid = anesrake_out$caseid, weights = anesrake_out$weightvec)

    data_return <- data |>
      # remove "old" weights with new weights
      dplyr::select(-weights) |>
      dplyr::left_join(caseweights, by = "caseid") |>
      # now we can drop the temporary id
      dplyr::select(-caseid)

    return(data_return)

  }

  # return list of imputed, matched/weighted and re-weighted datasets (mirwds)
  mirwds <- lapply(
    X = 1:length(x$models),
    FUN = estimate_raking_weights
    )

  return(mirwds)

}
