#' checkVariablesNames checks reporting and mappings on inconsistency in variable names
#'
#' Pass a vector of variable names (including the units if withunits=TRUE) or
#' a quitte object.
#' Get warnings if inconsistencies are found for the reporting
#' @param vars vector with variable names (and units such as "PE (EJ)")
#' @param withunits should the var vector contain units in paranthesis?
#' @return vector with all variables that show issues
#' @author Oliver Richters
#' @importFrom quitte is.quitte
#' @export
checkVarNames <- function(vars, withunits = TRUE) {
  if (is.quitte(vars)) vars <- unique(paste0(vars$variable, if (withunits) paste0(" (", vars$unit, ")")))

  barspace <- unique(grep("[\\| ]{2}|^[\\| ]|[\\| ]$", vars, value = TRUE))
  if (length(barspace) > 0) {
    warning("These variable names have wrong bars and spaces: ", paste(barspace, collapse = ", "))
  }

  naVar <- unique(grep("[\\|\\( ]NA[\\|\\) ]|^NA", vars, value = TRUE))
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

  return(sort(unique(c(barspace, naVar, noUnit))))
}
