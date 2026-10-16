# Global variables used in the package
#' @importFrom utils globalVariables
globalVariables(c(
  # agreement_metrics variables
  "RCT", "RWE", "significance_agreement", "estimate_agreement",
  "smd_agreement", "smd_value", "rct_estimate", "rct_lower", "rct_upper",
  "rwe_estimate", "rwe_lower", "rwe_upper", "where",

  # cox_pooling variables
  "weights", "subclass",

  # create_table1 variables
  "conf.low",

  # km_pooling variables
  "strata", "time", "u", "qbar", "se", "desc", "surv",
  "lower", "upper", "std.err", "estimate", "std.error",

  # raking_weights variables
  "caseid",

  # simulate_data variables
  "dem_age_index_cont", "dem_sex_cont", "c_smoking_history",
  "c_number_met_sites", "c_hemoglobin_g_dl_cont",
  "c_urea_nitrogen_mg_dl_cont", "c_platelets_10_9_l_cont",
  "c_calcium_mg_dl_cont", "c_glucose_mg_dl_cont",
  "c_lymphocyte_leukocyte_ratio_cont", "c_alp_u_l_cont",
  "c_protein_g_l_cont", "c_alt_u_l_cont", "c_albumin_g_l_cont",
  "c_bilirubin_mg_dl_cont", "c_chloride_mmol_l_cont",
  "c_monocytes_10_9_l_cont", "c_eosinophils_leukocytes_ratio_cont",
  "c_ldh_u_l_cont", "c_hr_cont", "c_sbp_cont", "c_oxygen_cont",
  "c_ecog_cont", "c_neutrophil_lymphocyte_ratio_cont", "c_bmi_cont",
  "c_ast_alt_ratio_cont", "c_stage_initial_dx_cont",
  "c_time_dx_to_index", "dem_race", "id", "eventtime", "status",
  "patientid",

  # design diagram
  "min_time", "max_time", "is_index_point", "dimension",
  "min_time_adj", "max_time_adj", "dimension_letter",
  "y_pos", "color", "superscript_letter", "time_window",
  "box_width", "text_width_estimate", "should_place_left",
  "text_fits", "mid_time", "text_x", "label_text",
  "text_hjust", "variable", "label", "variables"
))

#' Convert pooling input to a list of data frames
#'
#' Internal helper that allows \code{km_pooling()} to work on a list of data
#' frames only while remaining backwards compatible with mimids and wimids objects.
#'
#' For mimids/wimids objects, the matched/weighted imputed datasets are extracted
#' and the survfit arguments that go with them (weights, cluster, robust) are
#' attached as attribute \code{survfit_defaults}. A deprecation notice is shown (once per
#' session) for these classes. Lists are returned unchanged.
#'
#' @param x mimids or wimids object, or a list of data frames
#'
#' @return list of data frames; for mimids/wimids with attribute \code{survfit_defaults}
#'
#' @noRd
as_dataset_list <- function(x) {
  is_mimids <- inherits(x, "mimids")
  is_wimids <- inherits(x, "wimids")

  if (is_mimids || is_wimids) {
    assertthat::assert_that(
      "weights" %in% names(MatchThem::complete(x)),
      msg = "<x> needs to contain a weights column"
    )
    if (is_mimids) {
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
    datasets <- MatchThem::complete(
      x,
      action = "all",
      all = FALSE,
      include = FALSE
    )

    # user notice (once per session to avoid flooding loops and simulations)
    obj_class <- if (is_mimids) "mimids" else "wimids"
    example_args <- if (is_mimids) {
      "weights = weights, cluster = subclass, robust = TRUE"
    } else {
      "weights = weights, robust = TRUE"
    }
    cli::cli_inform(
      c(
        "!" = "Passing {.cls {obj_class}} objects to {.fn km_pooling} is deprecated.",
        "i" = "{.fn km_pooling} now works on a list of data frames. Your object was converted internally by the helper {.fn as_dataset_list}; this conversion is only kept for backwards compatibility and may be removed in a future release.",
        "i" = "Instead, pass {.code MatchThem::complete(x, action = \"all\", all = FALSE, include = FALSE)} and specify the survfit arguments via {.arg ...}, e.g. {.code {example_args}}."
      ),
      .frequency = "once",
      .frequency_id = "encore.analytics_as_dataset_list_deprecated"
    )

    # weights (and cluster membership for matching) with robust variance
    survfit_defaults <- list(weights = quote(weights), robust = TRUE)
    if (is_mimids) {
      survfit_defaults$cluster <- quote(subclass)
    }
    attr(datasets, "survfit_defaults") <- survfit_defaults

    return(datasets)
  }

  assertthat::assert_that(
    is.list(x) && !is.data.frame(x) && !inherits(x, "mids"),
    msg = "<x> needs to be a list of data frames, or a mimids or wimids object"
  )

  x
}
