#' Provide information on variable, its mappings and summation groups
#'
#' @md
#' @author Oliver Richters
#' @param varname string with variable name
#' @param mif filename of miffile
#' @param mapping vector of mapping shortcuts (AR6, NAVIGATE) or mapping filenames. NULL means all
#' @param remindVar column name of variable in mapping (default: piam_variable)
#' @param remindUnit column name of unit in mapping (default: piam_unit)
#' @importFrom quitte read.quitte as.quitte
#' @importFrom stringr str_split str_pad
#' @importFrom utils head
#' @return prints human-readable summary to the user
#' @examples
#' # Simple use. prints human-readable summary to the reader on Emi|CO2:
#' variableInfo(
#'   "Emi|CO2"
#' )
#' @export
variableInfo <- function(varname, mif = NULL, mapping = NULL, remindVar = "piam_variable", remindUnit = "piam_unit") {
  if (is.null(mapping)) mapping <- names(mappingNames())
  green <- "\033[0;32m"
  blue  <- "\033[0;34m"
  nc    <- "\033[0m"   # No Color
  width <- 60

  .getChilds <- function(v, c) {
    tobefound <- paste0("^", gsub("|", "\\|", removePlus(v), fixed = TRUE), "\\|[^\\|]*$")
    c[which(grepl(tobefound, removePlus(c)))]
  }

  varname <- trimws(varname)

  message("\n##### Search for information on ", green, varname, nc, " in mappings")
  for (m in mapping) {
    mappingData <- getMapping(m)
    mappingName <- basename(m)
    remindno <- which(removePlus(varname) == removePlus(mappingData[, remindVar]))
    exportno <- head(which(removePlus(varname) == removePlus(mappingData$variable)), n = 1)
    if (length(remindno) + length(exportno) == 0) {
      message("\n### Nothing found in mapping: ", blue, mappingName, nc)
      next
    } else {
      message("\n### Results from mapping: ", blue, mappingName, nc)
    }
    if (m %in% names(summationsNames())) {
      summationGroups <- getSummations(m)
      # print table column names
      summationsFile <- gsub(".*piamInterfaces", "piamInterfaces", summationsNames(m))
      message(str_pad(paste0("# Export variable groups found in ",
                           basename(summationsFile)), width + 8, "right"),
            "# Corresponding ", remindVar, " variables")
    } else {
      summationGroups <- NULL
      message("# No corresponding summation groups file found, show variables in mapping.")
      message("\n", str_pad(paste0("# Export variable"), width + 8, "right"),
            "# Corresponding ", remindVar, " variables")
    }
    for (no in unique(c(remindno, exportno))) {
      exportname <- mappingData$variable[no]
      remindname <- mappingData[no, remindVar]
      allexportchilds <- unique(.getChilds(exportname, mappingData$variable))
      allremindchilds <- unique(.getChilds(varname, mappingData[, remindVar]))
      if (exportname %in% summationGroups$parent) {
        exportchilds <- summationGroups$child[summationGroups$parent == exportname]
        # print summation parents
        message(". ", str_pad(paste(exportname, "="), width + 3, "right"), "   . ",
                paste0(mappingData[, remindVar][mappingData$variable == exportname], collapse = " + "), " =")
        # print summation childs
        for (ch in exportchilds) {
          remindchilds <- mappingData[, remindVar][mappingData$variable == ch]
          message("  + ", str_pad(ch, width + 2, "right"), "    + ", paste0(remindchilds, collapse = " + "))
          allremindchilds <- setdiff(allremindchilds, remindchilds)
        }
        allexportchilds <- setdiff(allexportchilds, exportchilds)
      } else {
        message(". ", str_pad(paste0(exportname, " (", mappingData$unit[no], ")"), width + 3, "right"),
                "   . ", remindname, " (", mappingData[, remindUnit][no], ")")
      }
      if (length(allexportchilds) + length(allremindchilds) > 0) {
        message("\n# Child variables", if (m %in% names(summationsNames())) " not in summation group")
        for (ch in allexportchilds) {
          remindchilds <- mappingData[, remindVar][mappingData$variable == ch]
          message("  . ", str_pad(ch, width + 1, "right"), "     . ", paste0(remindchilds, collapse = " + "))
          allremindchilds <- setdiff(allremindchilds, remindchilds)
        }
        for (ch in allremindchilds) {
          exportchild <- unique(mappingData$variable[mappingData[, remindVar] == ch])
          exportchild <- exportchild[! is.na(exportchild)]
          message("   . ", str_pad(paste(exportchild, collapse = ", "), width, "right"), "     . ", ch)
        }
      }
      message("Units: ", str_pad(paste0(unique(mappingData$unit[no]), collapse = ", "), width, "right"),
              " Units: ", paste0(unique(mappingData[, remindUnit][no]), collapse = ", "))
    }
  }
  if (! is.null(mif)) {
    mifdata <- quitte::as.quitte(mif)
    message("\n### Variables found in mif file")
    mifchilds <- .getChilds(varname, sort(unique(mifdata$variable)))
    for (ch in mifchilds) {
      message("- ", ch)
    }
  }

  csvdata <- system.file("renamed_piam_variables.csv", package = "piamInterfaces") %>%
    read.csv2(comment.char = "#", strip.white = TRUE) %>%
    as_tibble() %>%
    filter(.data$piam_variable %in% varname | .data$old_name %in% varname)
  if (nrow(csvdata) > 0) {
    message("\n### Renaming found in renamed_piam_variables.csv:\n",
            paste0("- ", csvdata$old_name, " -> ", csvdata$piam_variable, collapse = "\n"))
  }
}
