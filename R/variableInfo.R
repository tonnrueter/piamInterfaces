#' Provide information on variable, its mappings and summation groups
#'
#' @md
#' @author Oliver Richters
#' @param varname string with variable name
#' @param mif filename of miffile
#' @param mapping vector of mapping shortcuts (AR6, NAVIGATE) or mapping filenames. NULL means all
#' @importFrom quitte read.quitte as.quitte
#' @importFrom magclass unitjoin
#' @importFrom stringr str_split str_pad
#' @importFrom utils head
#' @return prints human-readable summary to the user
#' @examples
#' # Simple use. prints human-readable summary to the reader on Emi|CO2:
#' variableInfo(
#'   "Emi|CO2"
#' )
#' @export
variableInfo <- function(varname, mif = NULL, mapping = NULL) {   # nolint: cyclocomp_linter.
  if (length(mapping) == 0) mapping <- names(mappingNames())
  green <- "\033[0;32m"
  blue  <- "\033[0;34m"
  nc    <- "\033[0m"   # No Color
  width <- 60

  .getChilds <- function(v, c, keepparent = FALSE) {
    tobefound <- paste0("^", gsub("|", "\\|", deletePlus(v), fixed = TRUE), "\\|", if (keepparent) "?", "[^\\|]*$")
    c[which(grepl(tobefound, deletePlus(c)))]
  }

  varname <- trimws(varname)

  message("\n##### Search for information on ", green, varname, nc, " in mappings")
  # loop through mappings
  for (m in mapping) {
    mappingData <- getMapping(m)
    mappingName <- basename(m)
    # find fitting variables in piam_variable and variable columns
    piamno <- which(deletePlus(varname) == deletePlus(mappingData[, "piam_variable"]))
    exportno <- head(which(deletePlus(varname) == deletePlus(mappingData$variable)), n = 1)
    nothingfound <- isTRUE(length(piamno) + length(exportno) == 0)
    message("\n### ", ifelse(nothingfound, "Nothing found in", "Results from"), " mapping: ", blue, mappingName, nc)
    if (nothingfound) next
    # check whether summation file with same name as mapping exists and load it
    if (m %in% names(summationsNames())) {
      summationGroups <- getSummations(m)
      # print table column names
      summationsFile <- gsub(".*piamInterfaces", "piamInterfaces", summationsNames(m))
      message(str_pad(paste0("# Export variable groups found in ",
                             basename(summationsFile)), width + 8, "right"),
              "# Corresponding sum of 'piam_variable'")
    } else {
      summationGroups <- NULL
      message("# No corresponding summation groups file found, show variables in mapping.")
      message("\n", str_pad(paste0("# Export variable"), width + 8, "right"),
              "# Corresponding 'piam_variable'")
    }
    # loop through all lines found
    for (no in unique(c(piamno, exportno))) {
      exportname <- mappingData$variable[no]
      # find child variables (example: PE|Oil is child of PE) for piam and export variables
      allexportchilds <- unique(.getChilds(exportname, mappingData$variable))
      allpiamchilds <- unique(.getChilds(varname, mappingData[, "piam_variable"]))
      # check whether variable is parent in one or more summation groups
      parentgroups <- intersect(c(exportname, paste(exportname, seq(10))), summationGroups$parent)
      # if not parent of a summation group, just print the variables
      if (length(parentgroups) == 0) {
        message(". ", str_pad(exportname, width + 3, "right"), "     ", sumNamesWithFactors(mappingData, exportname))
      } else {
        for (parentname in parentgroups) {
          exportchilds <- summationGroups$child[summationGroups$parent %in% parentname]
          # print summation parents
          message("\n. ", str_pad(paste(parentname, "="), width + 3, "right"), "   ",
                  sumNamesWithFactors(mappingData, exportname), " =")
          # print summation childs
          for (ch in exportchilds) {
            piamchilds <- mappingData[, "piam_variable"][mappingData$variable %in% ch]
            message("  + ", str_pad(ch, width + 2, "right"), "    ", sumNamesWithFactors(mappingData, ch))
            allpiamchilds <- setdiff(allpiamchilds, piamchilds)
          }
          allexportchilds <- setdiff(allexportchilds, exportchilds)
        }
      }
      # print remaining childs not in summation group (or if no group exists)
      if (length(allexportchilds) + length(allpiamchilds) > 0) {
        message("\n# Child variables", if (m %in% names(summationsNames())) " not in summation group")
        for (ch in allexportchilds) {
          piamchilds <- mappingData[, "piam_variable"][mappingData$variable %in% ch]
          message("  . ", str_pad(ch, width + 1, "right"), "   = ", sumNamesWithFactors(mappingData, ch))
          allpiamchilds <- setdiff(allpiamchilds, piamchilds)
        }
        for (ch in allpiamchilds) {
          exportchild <- unique(mappingData$variable[mappingData[, "piam_variable"] %in% ch])
          exportchild <- exportchild[! is.na(exportchild)]
          for (x in exportchild) {
            message("  . ", str_pad(x, width + 1, "right"), "     . ", sumNamesWithFactors(mappingData, x))
          }
        }
      }
      # print units
      message("Units: ", str_pad(paste0(unique(mappingData$unit[no]), collapse = ", "), width, "right"),
              " Units: ", paste0(unique(mappingData[, "piam_unit"][no]), collapse = ", "))
      # print definitions if existing
      if ("description" %in% colnames(mappingData) && ! is.na(mappingData$description[[no]])) {
        message("\ndescription of ", unitjoin(mappingData$variable[[no]], mappingData$unit[[no]]), ": ",
                mappingData$description[[no]])
      }
    }
  }
  if (length(mif) > 0) {
    mifdata <- quitte::as.quitte(mif)
    message("\n### Variables found in mif file")
    mifchilds <- .getChilds(varname, sort(unique(mifdata$variable)), keepparent = TRUE)
    message(paste0("- ", mifchilds, collapse = "\n"))
  }

  match <- function(x) {
    startsWith(varname, sub("\\*$", "", deletePlus(x)))
  }
  csvdata <- system.file("renamed_piam_variables.csv", package = "piamInterfaces") %>%
    read.csv2(comment.char = "#", strip.white = TRUE) %>%
    as_tibble() %>%
    filter(unlist(lapply(.data$piam_variable, match)) | unlist(lapply(.data$old_name, match)))
  if (nrow(csvdata) > 0) {
    message("\n### Renaming found in renamed_piam_variables.csv:\n",
            paste0("- ", csvdata$old_name, " -> ", csvdata$piam_variable, collapse = "\n"))
  }
}
