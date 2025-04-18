#' Extract reference year for price indices from unit
#'
#' @md
#' @author Oliver Richters
#' @param unit vector or string of units such as 'Index (2020 = 1)'
#' @param var optional string of variable name to facilitate debugging
#' @return vector or string of reference years such as '2020'
#' @export

extractReferenceYear <- function(unit, var = NULL) {
  if (all(grepl("^Index \\([0-9]* = 1\\)", unit))) {
    referenceYear <- gsub("^Index \\(", "", gsub(" = 1\\)", "", unit))
    return(referenceYear)
  } else {
    stop("Unit ", paste(unit, collapse = ", "), if (! is.null(var)) paste0(" for ", var),
         " contains strings not of the form 'Index (2000 = 1)'.")
  }
}
