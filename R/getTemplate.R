#' Retrieves latest template for a given project
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param project name of the project of requested template
#' @importFrom utils read.csv2
#' @importFrom gms chooseFromList
#' @export
getTemplate <- function(project = NULL) {
  templates <- getTemplateNames()
  if (! isTRUE(project %in% names(templates))) {
    if (! is.null(project)) message(project, " not found and skipped.")
    project <- chooseFromList(names(templates), type = "project templates", returnBoolean = FALSE, multiple = FALSE)
  }
  if (length(project) == 0) {
    stop("Nothing selected, abort.")
  } else {
    return(read.csv2(templates[project], header = TRUE, sep = ";", na.strings = list("")))
  }
}
