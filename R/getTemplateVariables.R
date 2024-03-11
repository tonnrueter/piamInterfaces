#' Retrieves all variables allocated to source potentially used
#' in mappings to project variables
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param project name of the project of requested template
#' @param sources model abbreviation(s) used in 'source' column.
#'        R = REMIND, M = MAgPIE, T = EDGE-T, B = Brick, C = Climate/MAGICC, TRUE = all
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#' getTemplateVariables("AR6", "RT")
#' @export
getTemplateVariables <- function(project, sources = TRUE) {
  template <- getTemplate(project) %>%
    filter(! is.na(.data$piam_variable)) %>%
    mutate("modelvars" = paste0(.data$piam_variable, " (", .data$piam_unit, ")"))
  if (! isTRUE(sources)) {
    if (! "source" %in% names(template)) {
      stop("Template ", project, " has no column 'source', please add it.")
    }
    sources <- unlist(strsplit(paste0(sources), split = ""))
    template <- template %>%
      filter(! is.na(.data$source), .data$source %in% sources)
  }
  modelvars <- unique(as.character(template$modelvars))
  return(modelvars)
}

#' legacy function to be used by remind2
#' @md
#' @param project name of the project of requested template
#' @export
getREMINDTemplateVariables <- function(project) {
  return(getTemplateVariables(project, "R"))
}
