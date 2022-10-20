#' Retrieves latest template for a given project
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param project name of the project of requested template, or template filename
#' @importFrom utils read.csv2
#' @importFrom gms chooseFromList
#' @export
getTemplate <- function(project = NULL) {
  templates <- templateNames()
  if (is.null(project)) {
    project <- chooseFromList(names(templates), type = "templates",
                              returnBoolean = FALSE, multiple = FALSE)
    if (length(project) == 0) stop("No template selected, abort.")
  }
  if (! file.exists(project)) {
    project <- gsub("\\.csv$", "", gsub("^mapping_template_", "", project))
  }
  filename <- if (project %in% names(templates)) templates[project] else project
  if (file.exists(filename)) {
    return(read.csv2(filename, header = TRUE, sep = ";", na.strings = list("")))
  } else {
    stop("Mapping file ", filename, " not found.")
  }
}
