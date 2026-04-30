#' Retrieves mapping file names
#'
#' @md
#' @author Oliver Richters
#' @param project name of the project of requested mapping. If not specified,
#'        all existing mappings will be returned
#' @return mapping file(s)
#' @export
mappingNames <- function(project = NULL) {
  mappings <- Sys.glob(file.path(system.file("mappings", package = "piamInterfaces"), "mapping_*.csv"))
  names(mappings) <- gsub("mapping_", "", gsub(".csv", "", basename(mappings)))
  if (! isTRUE(project %in% names(mappings))) {
    return(mappings)
  } else {
    return(mappings[project])
  }
}

#' for backwards compatibility
#' @inheritParams mappingNames
#' @export
templateNames <- function(project = NULL) return(mappingNames(project))
