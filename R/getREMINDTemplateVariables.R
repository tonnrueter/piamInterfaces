#' Retrieves all REMIND variables potentially used in mappings to project variables
#'
#' @md
#' @author Falk Benke
#' @importFrom dplyr %>%
#' @export
getREMINDTemplateVariables <- function() {
  remindVars <- NULL
  for (t in templateNames()) {
    template <- getTemplate(t)

    if ("exclude_remind2_validation" %in% names(template)) {
      template <- template %>%
        filter(is.na(!!sym("exclude_remind2_validation")), !is.na(!!sym("r30m44"))) %>%
        mutate(!!sym("variable") := paste0(!!sym("r30m44"), " (", !!sym("r30m44_unit"), ")"))
      remindVars <- c(remindVars, as.character(template[, "variable"]))
    }
  }
  return(unique(remindVars))
}
