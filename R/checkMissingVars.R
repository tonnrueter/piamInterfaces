#' Check whether all variables required by mapping are present in mifdata for given source
#'
#' @md
#' @author Oliver Richters
#' @param mifdata data that can be read with as.quitte
#' @param mapping name of the project of requested mappings
#'        such as c('AR6', 'AR6_NGFS') or 'mapping.csv'. Use TRUE for all mappings.
#' @param sources model abbreviation(s) used in 'source' column.
#'        R = REMIND, M = MAgPIE, T = EDGE-T, B = Brick, C = Climate/MAGICC, TRUE = all
#' @importFrom piamutils deletePlus
#' @importFrom quitte as.quitte
#' @return an invisible vector with missing variables
#' @export
checkMissingVars <- function(mifdata, mapping = TRUE, sources = TRUE) {
  mifdata <- as.quitte(mifdata)
  mappingVariables <- deletePlus(getMappingVariables(mapping, sources))
  computedVariables <- unique(paste0(deletePlus(mifdata$variable), " (", gsub("^$", "unitless", mifdata$unit), ")"))
  missingVariables <- sort(setdiff(mappingVariables, computedVariables))
  if (length(missingVariables) > 0) {
    message("# The following ", length(missingVariables), " variables are expected in the piamInterfaces package",
            if (! isTRUE(mapping)) paste0(" for mapping ", paste(mapping, collapse = ", ")),
            if (! isTRUE(sources)) paste0(" with source ", sources),
            ", but cannot be found in the reporting:\n- ", paste(missingVariables, collapse = ",\n- "), "\n")
  }
  return(invisible(missingVariables))
}
