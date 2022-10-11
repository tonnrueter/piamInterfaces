#' Retrieves latest template for a given project
#'
#' @md
#' @author Falk Benke
#' @param project name of the project of requested template
#' @importFrom data.table fread
#' @export
getTemplate <- function(project) {
  templates <- c(
    "AR6" = "mapping_template_AR6.csv",
    "NAVIGATE" = "mapping_template_NAVIGATE.csv",
    "SHAPE" = "mapping_template_SHAPE.csv"
  )

  if (!project %in% names(templates)) {
    stop(paste0("Invalid project, currently supported: ", paste0(unlist(names(templates)), collapse = ", ")))
  }

  return(fread(paste0("inst/templates/", templates[project]), sep = ";", ))
}
