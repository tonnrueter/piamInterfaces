#' Retrieves summation group file names
#'
#' @md
#' @author Oliver Richters
#' @param project name of the project of requested summation file. If not specified,
#'        all existing summation files will be returned
#' @return template file(s)
#' @export
summationsNames <- function(project = NULL) {
  summation <- Sys.glob(file.path(system.file("summations", package = "piamInterfaces"), "summation_groups*.csv"))
  names(summation) <- gsub("summation_groups_", "", gsub(".csv", "", basename(summation)))
  if (! isTRUE(project %in% names(summation))) {
    return(summation)
  } else {
    return(summation[project])
  }
}
