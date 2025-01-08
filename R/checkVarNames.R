#' checkVariablesNames checks reporting and mappings on inconsistency in variable names
#'
#' Pass a vector of variable names (including the units if withunits=TRUE).
#' Get warnings if inconsistencies are found for the reporting
#' @param vars vector with variable names (and units such as "PE (EJ)")
#' @param withunits should the var vector contain units in paranthesis?
#' @author Oliver Richters
#' @export
checkVarNames <- function(vars, withunits = TRUE) {
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

  return(c(barspace, naVar, noUnit))
}
