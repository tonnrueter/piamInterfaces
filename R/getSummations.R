#' Retrieves latest summation group file for a given project
#'
#' @md
#' @author Oliver Richters
#' @param project name of the project of requested summation group, or summation group filename
#'        which can be a csv file of a xlsx file where the first sheet containing a 'variable' column
#'        is expected to have a 'components' column as well.
#'        If project is a https://files.ece.iiasa.ac.at/*.xlsx  URL, it will be automatically downloaded.
#' @param format either "dataframe" or "list", the latter ignores the factor column
#' @importFrom utils read.csv2
#' @importFrom gms chooseFromList
#' @importFrom dplyr filter mutate select rename
#' @importFrom tidyr pivot_longer separate_wider_delim
#' @importFrom tidyselect starts_with
#' @export
getSummations <- function(project = NULL, format = "dataframe") {
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
  if (file.exists(filename) || grepl("^https:\\/\\/files\\.ece\\.iiasa\\.ac\\.at\\/.*\\.xlsx$", filename)) {
    if (str_sub(filename, -5, -1) == ".xlsx") {
      template <- loadIIASATemplate(filename)
      if (! "components" %in% names(template)) stop("project=", filename, " is missing 'components' column.")
      summations <- template %>%
        select("variable", "components") %>%
        filter(! is.na(.data$components)) %>%
        mutate(components = gsub('\\\", \"', ",", gsub('^\\[\\\"|\\\"\\]$', "", .data$components))) %>%
        separate_wider_delim("components", ",", names_sep = "", too_few = "align_start") %>%
        pivot_longer(starts_with("components"), values_to = "child", names_to = NULL) %>%
        rename(parent = "variable")
    } else {
      summations <- read.csv2(filename, sep = ";", stringsAsFactors = FALSE,
                              strip.white = TRUE, comment.char = "#") %>%
      filter(! .data$parent %in% "")
    }
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
