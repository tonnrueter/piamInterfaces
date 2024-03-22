#' getMapping
#'
#' Retrieves latest mapping for a given project.
#' Mappings must contain the columns "variable", "unit", "piam_variable",
#' "piam_unit", "piam_factor".
#' Mappings are csv files with semicolon as a separator and no quotation marks
#' around fields, see main README.Rd file
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param project name of requested mapping, or file name pointing to a mapping
#' @importFrom utils read.csv2
#' @importFrom gms chooseFromList
#' @examples
#' \dontrun{
#' getMapping("ECEMF")
#' getMapping("/path/to/mapping/file")
#' }
#' @export
getMapping <- function(project = NULL) {
  mappings <- mappingNames()
  if (is.null(project)) {
    project <- chooseFromList(names(mappings), type = "mappings",
                              returnBoolean = FALSE, multiple = FALSE)
    if (length(project) == 0) stop("No mapping selected, abort.")
  }
  if (! file.exists(project)) {
    project <- gsub("\\.csv$", "", gsub("^mapping_", "", project))
  }
  filename <- if (project %in% names(mappings)) mappings[project] else project
  if (file.exists(filename)) {
    data <- read.csv2(filename, header = TRUE, sep = ";", na.strings = list(""),
                      strip.white = TRUE, quote = "", comment.char = "#")

    requiredCols <- c("variable", "unit", "piam_variable", "piam_unit", "piam_factor")

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

#' for backwards compatibility
#' @inheritParams getMapping
#' @export
getTemplate <- function(project = NULL) return(getMapping(project))
