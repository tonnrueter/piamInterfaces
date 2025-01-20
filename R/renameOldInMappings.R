#' renameOldInMappings
#'
#' Loads "renamed_piam_variables.csv" and replaces all instances of old_name
#' in the piam_variable columns of all mapping files
#' Handle with care, better commit your changes before using it
#' Best run this in your piamInterfaces folder:
#' Rscript -e 'devtools::load_all(); renameOldInMappings()'
#'
#' @md
#' @author Oliver Richters
#' @param folder piamInterfaces folder
#' @importFrom piamutils deletePlus
#' @importFrom stringr str_split
#' @export
renameOldInMappings <- function(folder = ".") {

  # find out where the csvfile is exactly and use that as a starting point
  csvfile <- "renamed_piam_variables.csv"
  csvfiles <- normalizePath(file.path(folder, c(".", "..", "inst", "../inst"), csvfile), mustWork = FALSE)
  folder <- head(dirname(csvfiles[file.exists(csvfiles)]), 1)
  if (is.null(folder)) stop(csvfile, " not found, specify 'folder' with such file.")

  # search files locally to avoid overwriting anything in renv or so
  mappingfiles <- list.files(file.path(folder, "mappings"), pattern = "mapping_.*\\.csv", full.names = TRUE)

  # collect all variables and expand renamed_piam_variables based on it
  vars <- getMappingVariables(addunit = FALSE)
  csvdata <- getExpandRenamedVariables(deletePlus(vars))

  # loop over mapping files
  for (m in mappingfiles) {
    mapping <- readLines(m)
    splitted <- str_split(mapping, ";")
    # col is number of piam_variable column
    col <- which(splitted[[1]] == "piam_variable")
    for (i in seq_along(splitted)) {
      if (length(splitted[[i]] >= col)) {
        replaceid <- which(csvdata$old_name == deletePlus(splitted[[i]][col]))
        if (length(replaceid) == 1) {
          message("In ", basename(m), ", replace: ", splitted[[i]][col], " -> ", csvdata$piam_variable[[replaceid]])
          splitted[[i]][col] <- csvdata$piam_variable[[replaceid]]
        }
      }
    }
    writeLines(unlist(lapply(splitted, paste0, collapse = ";")), con = m)
  }
}
