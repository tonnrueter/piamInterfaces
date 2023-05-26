#' Retrieves all REMIND variables potentially used in mappings to project variables
#'
#' @md
#' @author Falk Benke
#' @param project name of the project of requested template
#' @importFrom dplyr %>%
#' @export
getREMINDTemplateVariables <- function(project) {
  template <- getTemplate(project)

  remindVars <- NULL

  if ("exclude_remind2_validation" %in% names(template)) {
    template <- template %>%
      filter(is.na(!!sym("exclude_remind2_validation")), !is.na(!!sym("piam_variable"))) %>%
      mutate(!!sym("variable") := paste0(!!sym("piam_variable"), " (", !!sym("piam_unit"), ")"))
    remindVars <- c(remindVars, as.character(template[, "variable"]))
  } else {
    stop("Template ", project, " does not support remind2 validation. \n
          Add a column `exclude_remind2_validation`!")
  }
  remindVars <- gsub("^Price\\|Marginal\\|", "Price|", unique(remindVars))
  remindVars <- gsub("\\|Moving Avg", "", remindVars)
  return(unique(remindVars))
}
