#' Retrieves latest summation group file for a given project
#'
#' @md
#' @author Oliver Richters
#' @param project name of the project of requested summation group, or summation group filename
#' @importFrom utils read.csv2
#' @importFrom gms chooseFromList
#' @export
getSummations <- function(project = NULL) {
  summations <- summationsNames()
  if (is.null(project)) {
    project <- chooseFromList(names(summations), type = "summation group files",
                              returnBoolean = FALSE, multiple = TRUE, addAllPattern = FALSE)
    if (length(project) == 0) stop("No summation group files selected, abort.")
  }
  if (! file.exists(project)) {
    project <- gsub("\\.csv$", "", gsub("^summation_groups_", "", project))
  }
  filename <- if (project %in% names(summations)) summations[project] else project
  if (file.exists(filename)) {
    return(read.csv2(filename, sep = ";", stringsAsFactors = FALSE))
  } else {
    stop("Summation group file ", filename, " not found.")
  }
}
