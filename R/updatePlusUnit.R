#' updatePlusUnit
#'
#' Loads data and replaces all variables with the |+| notation used in this data.
#' Pick a mif file or quitte object and run:
#' Rscript -e 'devtools::load_all(); updatePlusUnit("your.mif")'
#'
#' @md
#' @author Oliver Richters
#' @param mif data file or quitte object
#' @param folder piamInterfaces folder
#' @importFrom quitte as.quitte
#' @importFrom piamutils deletePlus
#' @importFrom stringr str_split
#' @importFrom tidyselect everything
#' @export
updatePlusUnit <- function(mif, folder = ".") {

  # find out where the csvfile is exactly and use that as a starting point
  mappingfolder <- normalizePath(file.path(folder, c(".", "..", "inst", "../inst"), "mappings"), mustWork = FALSE)
  folder <- head(mappingfolder[file.exists(mappingfolder)], 1)
  if (is.null(folder)) stop("mappings folder not found, specify 'folder'.")

  # search files locally to avoid overwriting anything in renv or so
  mappingfiles <- list.files(mappingfolder, pattern = "mapping_.*\\.csv", full.names = TRUE)

  message("Loading data...")
  d <- distinct(select(as.quitte(mif), c("variable", "unit"))) %>%
    rename(mifunit = "unit", mifvariable = "variable") %>%
    filter(! .data$mifvariable %in% "Population") %>% # has different unit in REMIND + MAgPIE
    mutate(varWithoutPlus = deletePlus(.data$mifvariable)) %>%
    mutate(across(everything(), as.character))
  for (m in mappingfiles) {
    message("Updating ", basename(m), "...")
    getMapping(m) %>%
      mutate(varWithoutPlus = deletePlus(.data$piam_variable)) %>%
      left_join(d, by = "varWithoutPlus") %>%
      mutate(piam_variable = ifelse(is.na(.data$mifvariable), as.character(.data$piam_variable), .data$mifvariable),
             piam_unit = ifelse(is.na(.data$mifunit), as.character(.data$piam_unit), .data$mifunit)) %>%
      select(-"mifvariable", -"mifunit", -"varWithoutPlus") %>%
      writeMapping(m)
  }
}
