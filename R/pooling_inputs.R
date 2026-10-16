#' Input types and model arguments of the pooling functions
#'
#' @description
#' Shared documentation of \code{\link{km_pooling}} and \code{\link{cox_pooling}}.
#' Both functions fit one model per imputed dataset and then pool the results with Rubin's rules.
#' This page describes which inputs they accept and which arguments are passed on to the
#' per-dataset model.
#'
#' @name pooling_inputs
#' @keywords internal
#' @md
#'
#' @param x The imputed datasets to analyze. One of:
#' - a list of data frames (one per imputed dataset), e.g., the output of
#'   \code{\link{raking_weights}} or of \code{MatchThem::complete(..., action = "all")}
#' - a weighted (\code{wimids}) or matched (\code{mimids}) object created with \pkg{MatchThem}
#' - an imputed (\code{mids}) object created with \pkg{mice}
#'
#' See the section "Input types and default model arguments" for how each input is handled.
#' @param ... Additional arguments passed on to the per-dataset model call
#' (\code{\link[ggsurvfit]{survfit2}} for \code{km_pooling()}, \code{\link[survival]{coxph}} for
#' \code{cox_pooling()}). They are used in addition to, and take precedence over, the default
#' arguments of the input type (see the section "Input types and default model arguments").
#' Only \code{formula} and \code{data} are set internally and cannot be supplied here.
#' Column names are written without quotes and are looked up in each dataset, e.g.
#' \code{weights = w} or \code{id = patientid}.
#'
#' @section Input types and default model arguments:
#'
#' The datasets are first brought into the form of a list of data frames. Depending on the type of
#' \code{x}, default model arguments are set:
#'
#' | Input \code{x} | How the datasets are obtained | \code{weights} | \code{cluster} |
#' |:---|:---|:---|:---|
#' | \code{wimids} | \code{MatchThem::complete(x, action = "all", all = FALSE, include = FALSE)} | \code{weights} | none |
#' | \code{mimids} | same as \code{wimids} | \code{weights} | \code{subclass} |
#' | \code{mids} | \code{mice::complete(x, action = "all", include = FALSE)} | none (unweighted) | none |
#' | list of data frames | used as supplied | none | none |
#'
#' **Lists of data frames are never interpreted automatically.** A list can come from many sources
#' (weighting, matching, raking, your own code) so the function cannot know which columns are meant
#' as weights or clusters. You state this explicitly through \code{...}, for example:
#' - weighted data: \code{weights = weights}
#' - matched data: \code{weights = weights, cluster = subclass}
#' - weights stored in a column with another name: \code{weights = w}
#'
#' If the datasets contain a column named \code{weights} or \code{subclass} that is not used
#' because no \code{weights} or \code{cluster} (or \code{id}) argument was specified, a warning
#' is raised that names the argument to add. Specify \code{weights = NULL} or \code{cluster = NULL}
#' to confirm that the column should deliberately be ignored.
#'
#' \strong{Important}: when you build the list from a \code{mimids} or \code{wimids} object yourself,
#' use \code{MatchThem::complete(x, action = "all", all = FALSE, include = FALSE)}. With
#' \code{all = TRUE}, unmatched patients and patients with zero weight are included in the models.
#'
#' @section Precedence of arguments in \code{...}:
#'
#' Default arguments of the input type are applied first, then the arguments you supply via
#' \code{...}. If both contain the same argument, \strong{your argument wins}. For example, for a
#' \code{mimids} object \code{cluster = NULL} fits the model without clustering on the matched sets
#' and \code{cluster = pair_id} clusters on another column. \code{formula} and \code{data} are the only
#' reserved arguments.
#'
#' \code{robust} is set automatically: unless you specify it, \code{robust = TRUE} is used whenever
#' weights, a cluster or an id are part of the model (the model-based variance is not valid in these
#' cases), otherwise the default of the model function is kept. An explicitly supplied \code{robust}
#' always takes precedence.
#'
#' @section Transparency:
#'
#' Every call prints a message that lists how the datasets were obtained and exactly which
#' arguments were used for the models, including where each argument comes from (default for the
#' input type, derived, or specified by you). Use \code{suppressMessages()} to silence it.
NULL


# prepare_pooling_input() -------------------------------------------------------

#' Turn the input of a pooling function into a list of datasets plus default arguments
#'
#' S3 generic with one small method per supported input type. Every method returns the same
#' three elements so that the pooling functions never need to look at the class of \code{x} again:
#' \itemize{
#'   \item \code{datasets}: plain list of data frames, one per imputed dataset
#'   \item \code{args}: named list of default model arguments as unevaluated expressions
#'     (e.g., \code{list(weights = quote(weights))}); column names stay symbols so that
#'     they are looked up in each dataset when the model is fitted
#'   \item \code{note}: one-line description of the input that is shown to the user
#' }
#' The methods have no side effects (no messages), which keeps them easy to test.
#'
#' @param x mimids, wimids or mids object, or a list of data frames
#'
#' @return list with elements \code{datasets}, \code{args} and \code{note}
#'
#' @noRd
prepare_pooling_input <- function(x) {
  UseMethod("prepare_pooling_input")
}

#' @noRd
#' @exportS3Method
prepare_pooling_input.wimids <- function(x) {
  list(
    datasets = complete_matched_weighted(x),
    # MatchThem always names the weight column "weights"
    args = list(weights = quote(weights)),
    note = "<wimids>: weighted datasets extracted with MatchThem::complete(action = \"all\", all = FALSE, include = FALSE)"
  )
}

#' @noRd
#' @exportS3Method
prepare_pooling_input.mimids <- function(x) {
  list(
    datasets = complete_matched_weighted(x),
    # MatchThem always names the weight column "weights" and the matched sets "subclass"
    args = list(weights = quote(weights), cluster = quote(subclass)),
    note = "<mimids>: matched datasets extracted with MatchThem::complete(action = \"all\", all = FALSE, include = FALSE)"
  )
}

#' @noRd
#' @exportS3Method
prepare_pooling_input.mids <- function(x) {
  list(
    # there are no weights or matched sets, so every observation is used
    datasets = mice::complete(x, action = "all", include = FALSE),
    args = list(),
    note = "<mids>: imputed datasets extracted with mice::complete(action = \"all\", include = FALSE); unweighted"
  )
}

#' @noRd
#' @exportS3Method
prepare_pooling_input.list <- function(x) {
  assertthat::assert_that(
    length(x) > 0 && all(vapply(x, is.data.frame, logical(1))),
    msg = "<x> needs to be a non-empty list of data frames"
  )

  list(
    datasets = x,
    # a list can come from anywhere (weighting, matching, raking, ...), so nothing
    # is assumed here; weights/cluster are specified by the user via `...`
    args = list(),
    note = "list of data frames supplied by the user; no weights or cluster applied automatically"
  )
}

#' @noRd
#' @exportS3Method
prepare_pooling_input.mild <- function(x) {
  # the output of complete(action = "all") has class "mild" (not "list"), but is
  # a plain list of data frames, so it is handled like one
  prepare_pooling_input.list(unclass(x))
}

#' @noRd
#' @exportS3Method
prepare_pooling_input.default <- function(x) {
  # e.g., a single data frame or a vector
  stop(
    "<x> needs to be a mimids, wimids or mids object or a list of data frames",
    call. = FALSE
  )
}

#' Extract the matched/weighted imputed datasets of a mimids or wimids object
#'
#' @param x mimids or wimids object
#'
#' @return list of data frames, one per imputed dataset
#'
#' @noRd
complete_matched_weighted <- function(x) {
  # MatchThem objects need to contain the columns that the default arguments refer to
  assertthat::assert_that(
    "weights" %in% names(MatchThem::complete(x)),
    msg = "<x> needs to contain a weights column"
  )
  if (inherits(x, "mimids")) {
    assertthat::assert_that(
      "subclass" %in% names(MatchThem::complete(x)),
      msg = "<x> needs to contain a subclass column"
    )
  }

  # action = "all"  : produces a mild object (list) of the multiply imputed datasets
  # all = FALSE     : do NOT include observations with a zero estimated weight
  # include = FALSE : do NOT include original data with the missing values

  # Important: the all = parameter needs to be set to FALSE
  # since otherwise unmatched patients or those with zero weight
  # will be included, too!
  MatchThem::complete(
    x,
    action = "all",
    all = FALSE,
    include = FALSE
  )
}


# fit_models() --------------------------------------------------------------------

#' Fit one model per imputed dataset
#'
#' Shared by \code{km_pooling()} and \code{cox_pooling()}. The steps are:
#' \enumerate{
#'   \item merge default arguments of the input type with the arguments of the user
#'   \item derive \code{robust} if the user did not specify it
#'   \item check the arguments against the data and warn about unused weights/subclass columns
#'   \item tell the user how the models are fitted
#'   \item fit the model in each dataset
#' }
#'
#' @param input output of \code{prepare_pooling_input()}
#' @param fit_fun unevaluated name of the model function, e.g., \code{quote(survival::coxph)}
#' @param model_label model name shown to the user, e.g., "Cox proportional hazards model"
#' @param formula model formula
#' @param user_args named list of the unevaluated arguments the user supplied via \code{...},
#'   captured with \code{as.list(substitute(list(...)))[-1]} in the calling function
#'
#' @return list with the fitted models (\code{fits}, one per dataset) and the arguments used (\code{args})
#'
#' @noRd
fit_models <- function(input, fit_fun, model_label, formula, user_args) {
  datasets <- input$datasets
  defaults <- input$args

  # 1. merge arguments -----------------------------------------------------------
  # defaults first, then the user's arguments; if both contain the same
  # argument, the user's argument replaces the default
  args <- c(defaults[setdiff(names(defaults), names(user_args))], user_args)

  # 2. derive robust -------------------------------------------------------------
  # a robust (sandwich) variance is required whenever weights, clusters or ids are used.
  # `is.null()` is used because weights = NULL / cluster = NULL mean "not used"
  is_used <- function(arg) !is.null(args[[arg]])
  derived_robust <- !("robust" %in% names(args)) &&
    any(vapply(c("weights", "cluster", "id"), is_used, logical(1)))
  if (derived_robust) {
    args$robust <- TRUE
  }

  # 3. checks --------------------------------------------------------------------
  check_columns_exist(args, datasets)
  warn_unused_columns(args, datasets)

  # 4. tell the user ------------------------------------------------------------
  report_fit_setup(
    input = input,
    model_label = model_label,
    fit_fun = fit_fun,
    formula = formula,
    args = args,
    user_args = user_args,
    derived_robust = derived_robust
  )

  # 5. fit the model in each dataset -------------------------------------------
  # arguments that are NULL (e.g., cluster = NULL) are dropped, they mean "not used"
  args <- args[!vapply(args, is.null, logical(1))]

  # The call is built with as.call()/eval() rather than written literally with a
  # trailing `...`. ggsurvfit::survfit2() reconstructs its call internally
  # (match.call(expand.dots = TRUE)) to dispatch to survival::survfit(); if `...` is
  # spliced in literally, that reconstruction degrades NSE arguments like `id` into
  # positional `..1`-style placeholders that no longer resolve once the model tries to
  # evaluate them as columns of `data = d`. Building the call explicitly keeps
  # `formula` and `data` as plain symbols and the remaining arguments (e.g.,
  # weights = weights) as unevaluated symbols, which the model function then resolves
  # against `data = d`, exactly like a literal call would.
  fit_one <- function(d) {
    call_expr <- as.call(c(
      fit_fun,
      list(formula = quote(formula), data = quote(d)),
      args
    ))
    eval(call_expr, envir = environment())
  }

  list(fits = lapply(datasets, FUN = fit_one), args = args)
}

#' Check that column names supplied as weights/cluster/id exist in the datasets
#'
#' @param args merged model arguments
#' @param datasets list of data frames
#'
#' @noRd
check_columns_exist <- function(args, datasets) {
  for (arg in intersect(c("weights", "cluster", "id"), names(args))) {
    # only bare column names (symbols) can be checked; NULL or expressions are left alone
    if (is.symbol(args[[arg]])) {
      column <- as.character(args[[arg]])
      missing_in <- !vapply(datasets, function(d) column %in% names(d), logical(1))
      if (any(missing_in)) {
        cli::cli_abort(c(
          "Column {.field {column}} specified via {.arg {arg}} not found in the datasets of {.arg x}.",
          "i" = "Specify the name of an existing column, e.g. {.code {arg} = <column>}."
        ))
      }
    }
  }
}

#' Warn if a weights or subclass column exists but no matching argument was specified
#'
#' @param args merged model arguments
#' @param datasets list of data frames
#'
#' @noRd
warn_unused_columns <- function(args, datasets) {
  # `%in% names(args)` (rather than is.null) so that weights = NULL / cluster = NULL
  # count as a deliberate choice and silence the warning
  columns <- names(datasets[[1]])
  fixes <- character()

  if ("weights" %in% columns && !"weights" %in% names(args)) {
    fixes <- c(fixes, "weights" = "weights = weights")
  }
  if ("subclass" %in% columns && !any(c("cluster", "id") %in% names(args))) {
    fixes <- c(fixes, "subclass" = "cluster = subclass")
  }

  if (length(fixes) > 0) {
    unused <- names(fixes)
    use_it <- paste(fixes, collapse = ", ")
    ignore_it <- paste(sub("=.*", "= NULL", fixes), collapse = ", ")
    cli::cli_warn(c(
      "!" = "Column{?s} {.field {unused}} found in {.arg x} but not used in the models.",
      "i" = "To use the column(s), specify {.code {use_it}}.",
      "i" = "To ignore the column(s) on purpose, specify {.code {ignore_it}}."
    ))
  }
}

#' Tell the user how the models are fitted
#'
#' @inheritParams fit_models
#' @param args merged model arguments (before NULL arguments are dropped)
#' @param derived_robust whether \code{robust} was set automatically
#'
#' @noRd
report_fit_setup <- function(input, model_label, fit_fun, formula, args, user_args, derived_robust) {
  # curly braces have a meaning in cli messages, so escape them in text we insert
  escape <- function(text) gsub("\\}", "}}", gsub("\\{", "{{", text))

  # where does each argument come from?
  source_label <- function(arg) {
    if (arg == "robust" && derived_robust) {
      "derived: weights, cluster or id are used"
    } else if (arg %in% names(user_args) && arg %in% names(input$args)) {
      paste0("specified by you, overrides default ", arg, " = ", deparse(input$args[[arg]]))
    } else if (arg %in% names(user_args)) {
      "specified by you"
    } else {
      "default for the input type"
    }
  }

  if (length(args) == 0) {
    arg_lines <- "no weights, cluster or other arguments used"
  } else {
    arg_lines <- vapply(
      names(args),
      function(arg) {
        paste0(arg, " = ", paste(deparse(args[[arg]]), collapse = ""), "   (", source_label(arg), ")")
      },
      character(1)
    )
  }

  m <- length(input$datasets)
  cli::cli_inform(c(
    "i" = escape(paste0("Input: ", input$note, " (m = ", m, ")")),
    "i" = escape(paste0("Fitting ", model_label, " with ", deparse(fit_fun), "() in each dataset:")),
    " " = escape(paste0("formula: ", paste(deparse(formula), collapse = " "))),
    stats::setNames(escape(arg_lines), rep(" ", length(arg_lines)))
  ))
}
