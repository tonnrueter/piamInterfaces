#' checkVariablesNames checks reporting and mappings on inconsistency in variable names
#'
#' Pass a vector of variable names (including the units if withunits=TRUE).
#' Get warnings if inconsistencies are found for the reporting
#' @param vars vector with variable names (and units such as "PE (EJ)")
#' @param withunits should the var vector contain units in paranthesis?
#' @param mappingName enables to define variablesWithoutUnit for specific mapping
#' @author Oliver Richters
#' @export
checkVarNames <- function(vars, withunits = TRUE, mappingName = NULL) {
  barspace <- unique(grep("[\\| ]{2}|^[\\| ]|[\\| ]$", vars, value = TRUE))
  if (length(barspace) > 0) {
    warning("These variable names have wrong bars and spaces: ", paste(barspace, collapse = ", "))
  }

  variablesWithoutUnit_ScenarioMIP <- c(
    "Gender Inequality Index (NA)",
    "Ocean|Carbonate Saturation|Aragonite (NA)",
    "Ocean|Carbonate Saturation|Calcite (NA)",
    "Political Institutions|Equality Before Law and Individual Liberty (NA)",
    "Terrestrial Biodiversity|Shannon Crop Diversity Index (NA)",
    NULL
  )

  if (is.null(mappingName) || !(mappingName == "ScenarioMIP")) {
    naVar <- unique(grep("[\\|\\( ]NA[\\|\\) ]|^NA", vars, value = TRUE))
  } else if (mappingName == "ScenarioMIP") {
    filteredVars <- vars[!vars %in% variablesWithoutUnit_ScenarioMIP]
    naVar <- unique(grep("[\\|\\( ]NA[\\|\\) ]|^NA", filteredVars, value = TRUE))
  }

  if (length(naVar) > 0) {
    warning("These variables and units contain NA: ", paste(naVar, collapse = ", "))
  }

  noUnit <- NULL
  if (withunits) {
    noUnit <- unique(grep(" \\(.*\\)$", vars, value = TRUE, invert = TRUE))
    if (length(noUnit) > 0) {
      warning("These variables have no units: ", paste(noUnit, collapse = ", "))
    }
  }

  return(c(barspace, naVar, noUnit))
}
