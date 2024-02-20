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
      mutate("variable" = paste0(.data$piam_variable, " (", .data$piam_unit, ")"))
    remindVars <- c(remindVars, as.character(template[, "variable"]))
  } else {
    stop("Template ", project, " does not support remind2 validation. \n
          Add a column `exclude_remind2_validation`!")
  }
  return(unique(remindVars))
}
