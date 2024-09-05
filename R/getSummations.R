#' Retrieves latest summation group file for a given project
#'
#' @md
#' @author Oliver Richters
#' @param project name of the project of requested summation group, or summation group filename
#' @param format either "dataframe" or "list", the latter ignores the factor column
#' @importFrom utils read.csv2
#' @importFrom gms chooseFromList
#' @export
getSummations <- function(project = NULL, format = "dataframe") {
  summations <- summationsNames()
  if (is.null(project)) {
    project <- chooseFromList(names(summations), type = "summation group files",
                              returnBoolean = FALSE, multiple = TRUE, addAllPattern = FALSE)
    if (length(project) == 0) stop("No summation group files selected, abort.")
  }
  if (!file.exists(project)) {
    project <- gsub("\\.csv$", "", gsub("^summation_groups_", "", project))
  }
  filename <- if (project %in% names(summations)) summations[project] else project
  if (file.exists(filename)) {
    summations <- read.csv2(filename, sep = ";", stringsAsFactors = FALSE,
                            strip.white = TRUE, comment.char = "#") %>%
      filter(! .data$parent %in% "")
    if (!("factor" %in% names(summations))) {
      summations$factor <- 1
    } else {
      summations$factor <- as.numeric(summations$factor)
    }
    if (isTRUE(format == "list")) {
      summationsList <- list()
      for (i in unique(summations$parent)) {
        summationsList[[i]] <- summations[summations[, "parent"] == i, "child"]
      }
      return(summationsList)
    } else {
      return(summations)
    }
  } else {
    stop("Summation group file ", filename, " not found.")
  }
}
