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
  "lower", "upper", "std.err",

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
