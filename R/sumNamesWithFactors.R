#' From mappingData, return the piam_variable sum as a string for a given exportname
#'
#' @md
#' @author Oliver Richters
#' @param mappingData mapping data as obtained by getMapping()
#' @param exportname name to be matched in 'variable' column
#' @param width can be NULL so everything is pasted after each other, or the width argument
#'        from checkSummations such that every element gets its own line
#' @export
sumNamesWithFactors <- function(mappingData, exportname, width = NULL) {
  remindnames <- mappingData[, "piam_variable"][mappingData$variable %in% exportname]
  remindfactors <- mappingData[, "piam_factor"][mappingData$variable %in% exportname]
  remindfactors[is.na(remindfactors) | remindfactors %in% c("", 1, "1")] <- "+"
  remindfactors[remindfactors %in% c(-1, "-1")] <- "-"
  remindfactors[remindfactors >= 0] <- paste("+", remindfactors[remindfactors > 0])
  if (is.null(width)) {
    thesum <- paste(remindfactors, remindnames, collapse = " ")
  } else {
    thesum <- paste(remindfactors, remindnames, collapse = paste0("\n", paste(rep(" ", width + 12), collapse = "")))
  }
  thesum <- sub("^\\+ NA$", "NA", thesum)
  return(thesum)
}
