#' Retrieves all variables allocated to source potentially used
#' in mappings to project variables
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param project name of the project of requested mapping
#'        such as c('AR6', 'AR6_NGFS') or 'mapping.csv'
#' @param sources model abbreviation(s) used in 'source' column.
#'        R = REMIND, M = MAgPIE, T = EDGE-T, B = Brick, C = Climate/MAGICC, TRUE = all
#' @importFrom dplyr %>% select filter mutate
#' @importFrom rlang .data
#' @importFrom tidyselect any_of
#' @examples
#' getMappingVariables("AR6", "RT")
#' @export
getMappingVariables <- function(project, sources = TRUE) {
  mapping <- NULL
  for (p in project) {
    mapping <- getMapping(p) %>%
      select(any_of(c("piam_variable", "piam_unit", "source"))) %>%
      filter(! is.na(.data$piam_variable)) %>%
      mutate("modelvars" = paste0(.data$piam_variable, " (", .data$piam_unit, ")")) %>%
      rbind(mapping)
  }
  if (! isTRUE(sources)) {
    if (! "source" %in% names(mapping)) {
      stop("Mapping ", project, " has no column 'source', please add it.")
    }
    # split RxMT into c("Rx", "M", "T")
    sources <- unlist(strsplit(paste0(sources), split = ""))
    sources <- setdiff(paste0(sources, ifelse(seq_along(sources) == which(sources == "x") - 1, "x", "")), "x")
    mapping <- mapping %>%
      filter(! is.na(.data$source), .data$source %in% sources)
  }
  modelvars <- sort(unique(as.character(mapping$modelvars)))
  return(modelvars)
}

#' legacy function to be used by remind2
#' @md
#' @param project name of the project of requested mapping
#' @export
getREMINDTemplateVariables <- function(project) {
  return(getMappingVariables(project, "R"))
}
