#' Retrieves latest template for a given project
#'
#' @md
#' @author Falk Benke
#' @param project name of the project of requested template
#' @importFrom utils read.csv2
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

  return(read.csv2(system.file("templates", templates[project], package = "piamInterfaces"),
                   header = TRUE, sep = ";", na.strings = list("")))
}
