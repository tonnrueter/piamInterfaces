#' Retrieves all REMIND variables potentially used in mappings to project variables
#'
#' @md
#' @author Falk Benke
#' @param project name of the project of requested template
#' @param excludeMAGICC exclude MAGICC variables used in template
#' @importFrom dplyr %>%
#' @export
getREMINDTemplateVariables <- function(project, excludeMAGICC = TRUE) {
  template <- getTemplate(project)

  remindVars <- NULL

  if ("exclude_remind2_validation" %in% names(template)) {
    template <- template %>%
      filter(is.na(!!sym("exclude_remind2_validation")), !is.na(!!sym("piam_variable"))) %>%
      mutate(!!sym("variable") := paste0(!!sym("piam_variable"), " (", !!sym("piam_unit"), ")"))
    remindVars <- c(remindVars, as.character(template[, "variable"]))
  } else {
    stop("Template ", project, " does not support remind2 validation. \n
          Add a column `exclude_remind2_validation`!")
  }

  if (excludeMAGICC == TRUE) {
    magiccVars <- c(
      "Concentration|CH4 (ppb)",
      "Concentration|CO2 (ppm)",
      "Concentration|N2O (ppb)",
      "Forcing (W/m2)",
      "Forcing|Aerosol (W/m2)",
      "Forcing|Aerosol|BC (W/m2)",
      "Forcing|Aerosol|Cloud Indirect (W/m2)",
      "Forcing|Aerosol|OC (W/m2)",
      "Forcing|Aerosol|Other (W/m2)",
      "Forcing|Aerosol|Sulfate Direct (W/m2)",
      "Forcing|Albedo Change and Mineral Dust (W/m2)",
      "Forcing|CH4 (W/m2)",
      "Forcing|CO2 (W/m2)",
      "Forcing|F-Gases (W/m2)",
      "Forcing|Kyoto Gases (W/m2)",
      "Forcing|Montreal Gases (W/m2)",
      "Forcing|N2O (W/m2)",
      "Forcing|Other (W/m2)",
      "Forcing|Tropospheric Ozone (W/m2)",
      "Temperature|Global Mean (K)"
    )
    remindVars <- setdiff(remindVars, magiccVars)
  }

  return(unique(remindVars))
}
