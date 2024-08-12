#' From mappingData, return the piam_variable sum as a string for a given exportname
#'
#' @md
#' @author Oliver Richters
#' @param mappingData mapping data as obtained by getMapping()
#' @param exportname name to be matched in 'variable' column
#' @export
sumNamesWithFactors <- function(mappingData, exportname) {
  remindnames <- mappingData[, "piam_variable"][mappingData$variable %in% exportname]
  remindfactors <- mappingData[, "piam_factor"][mappingData$variable %in% exportname]
  remindfactors[is.na(remindfactors) | remindfactors %in% c("", 1, "1")] <- "+"
  remindfactors[remindfactors %in% c(-1, "-1")] <- "-"
  remindfactors[remindfactors > 0] <- paste("+", remindfactors[remindfactors > 0])
  thesum <- paste(remindfactors, remindnames, collapse = " ")
  thesum <- sub("^\\+ NA$", "NA", thesum)
  return(thesum)
}
