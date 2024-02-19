#' Generate valid path to logFile and make sure the outputDirectory exists.
#' If logFile is just a file name without any further path info, put logFile in outputDirectory
#'
#' @md
#' @author Oliver Richters
#' @param outputDirectory path to directory to place generated files
#' @param logFile path or name for log file
setLogFile <- function(outputDirectory = NULL, logFile = NULL) {
  if (! is.null(logFile) && ! isFALSE(logFile)) {
    if (is.character(logFile) && basename(logFile) == logFile) {
      logFile <- file.path(outputDirectory, logFile)
    }
    dir.create(dirname(logFile), recursive = TRUE, showWarnings = FALSE)
  }
  if (! is.null(outputDirectory)) {
    dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)
  }
  return(logFile)
}
