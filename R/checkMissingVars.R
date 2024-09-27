#' Check whether all variables required by mapping are present in mifdata for given source
#'
#' @md
#' @author Oliver Richters
#' @param mifdata data that can be read with as.quitte
#' @param mapping list of mapping that can be passed to getMappingVariables
#' @param sources source than can be passed to getMappingVariables
#' @importFrom piamutils deletePlus
#' @importFrom quitte as.quitte
#' @return an invisible vector with missing variables
#' @export
checkMissingVars <- function(mifdata, mapping, sources = TRUE) {
  mifdata <- as.quitte(mifdata)
  mappingVariables <- deletePlus(getMappingVariables(mapping, sources))
  computedVariables <- unique(paste0(deletePlus(mifdata$variable), " (", gsub("^$", "unitless", mifdata$unit), ")"))
  missingVariables <- sort(setdiff(mappingVariables, computedVariables))
  if (length(missingVariables) > 0) {
    message("# The following ", length(missingVariables), " variables are expected in the piamInterfaces package ",
            "for mapping ", paste(mapping, collapse = ", "), " and source ", sources,
            ", but cannot be found in the reporting:\n- ", paste(missingVariables, collapse = ",\n- "), "\n")
  }
  return(invisible(missingVariables))
}
