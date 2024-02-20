#' getTemplate
#'
#' Retrieves latest template for a given project.
#' Templates must contain the columns "Variable", "Unit", "piam_variable",
#' "piam_unit", "piam_factor".
#' Templates are csv files with semicolon as a separator and no quotation marks
#' around fields.
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param project name of requested template, or file name pointing to a template
#' @importFrom utils read.csv2
#' @importFrom gms chooseFromList
#' @examples
#' \dontrun{
#' getTemplate("ECEMF")
#' getTemplate("/path/to/template/file")
#' }
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
    data <- read.csv2(filename, header = TRUE, sep = ";", na.strings = list(""), strip.white = TRUE, quote = "")

    requiredCols <- c("Variable", "Unit", "piam_variable", "piam_unit", "piam_factor")

    if (length(data) == 1) {
      stop(paste0("Failed to read in ", filename, ". Is source file separated by semicolons?"))
    }

    if (!all(requiredCols %in% colnames(data))) {
      stop(paste0("Failed to read in ", filename, ". Required columns not found: ",
                  paste0(setdiff(requiredCols, colnames(data)), collapse = ", ")))
      }

    return(data)
  } else {
    stop("Mapping file ", filename, " not found.")
  }
}
