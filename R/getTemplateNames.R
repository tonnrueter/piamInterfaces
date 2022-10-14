#' Retrieves template file names
#'
#' @md
#' @author Oliver Richters
#' @param project name of the project of requested template, else will return all available templates.
#' @return template file(s)
#' @export
getTemplateNames <- function(project = NULL) {
  templates <- Sys.glob(file.path(system.file("templates", package = "piamInterfaces"), "mapping_template*.csv"))
  names(templates) <- gsub("mapping_template_", "", gsub(".csv", "", basename(templates)))
  if (! isTRUE(project %in% names(templates))) {
    return(templates)
  } else {
    return(templates[project])
  }
}
