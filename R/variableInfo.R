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
  minwidth <- 60       # minimal width of first column

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
    piamno <- which(deletePlus(varname) == deletePlus(mappingData$piam_variable))
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
                             basename(summationsFile)), minwidth + 8, "right"),
              "# Corresponding sum of 'piam_variable'")
    } else {
      summationGroups <- NULL
      message("# No corresponding summation groups file found, show variables in mapping.")
      message("\n", str_pad(paste0("# Export variable"), minwidth + 8, "right"),
              "# Corresponding 'piam_variable'")
    }
    # loop through all lines found
    allexportchilds <- NULL
    printedexportchilds <- NULL
    allpiamchilds <- NULL
    printedpiamchilds <- NULL
    for (no in unique(c(piamno, exportno))) {
      exportname <- mappingData$variable[no]
      # find child variables (example: PE|Oil is child of PE) for piam and export variables
      allexportchilds <- unique(c(allexportchilds, .getChilds(exportname, mappingData$variable)))
      width <- max(minwidth, max(nchar(c(exportname, allexportchilds))))
      allpiamchilds <- unique(c(allpiamchilds, .getChilds(varname, mappingData$piam_variable)))
      # check whether variable is parent in one or more summation groups
      parentgroups <- unique(c(intersect(c(exportname, paste(exportname, seq(10))), summationGroups$parent),
                               summationGroups$parent[summationGroups$child == exportname]))
      # if not parent of a summation group, just print the variables
      if (length(parentgroups) == 0) {
        allexportchilds <- unique(c(exportname, allexportchilds))
      } else {
        for (parentname in parentgroups) {
          message(paste(printSumGroup(summationGroups, mappingData, parentname, "=", width), collapse = "\n"))
          exportchilds <- summationGroups$child[summationGroups$parent %in% parentname]
          printedpiamchilds <- c(printedpiamchilds, mappingData$piam_variable[mappingData$variable %in% exportchilds])
          printedexportchilds <- c(printedexportchilds, exportchilds)
        }
      }
    }
    # print remaining childs not in summation group (or if no group exists)
    allexportchilds <- sort(setdiff(allexportchilds, printedexportchilds))
    allpiamchilds <- sort(setdiff(allpiamchilds, printedpiamchilds))
    if (length(allexportchilds) + length(allpiamchilds) > 0) {
      message("\n# Child variables", if (m %in% names(summationsNames())) " not in summation group")
      for (ch in allexportchilds) {
        piamchilds <- mappingData[, "piam_variable"][mappingData$variable %in% ch]
        message("  . ", str_pad(ch, width + 3, "right"), "   = ",
                gsub("^\\+ ", "", sumNamesWithFactors(mappingData, ch)))
        allpiamchilds <- setdiff(allpiamchilds, piamchilds)
      }
      for (ch in allpiamchilds) {
        exportchild <- unique(mappingData$variable[mappingData[, "piam_variable"] %in% ch])
        exportchild <- exportchild[! is.na(exportchild)]
        for (x in exportchild) {
          message("  . ", str_pad(x, width + 3, "right"), "   = ",
                  gsub("\\+ ", "", sumNamesWithFactors(mappingData, x)))
        }
      }
    }
    # print units
    unitno <- unique(c(piamno, exportno))
    message("Units: ", str_pad(paste0(unique(mappingData$unit[unitno]), collapse = ", "), width, "right"),
            " Units: ", paste0(unique(mappingData[, "piam_unit"][unitno]), collapse = ", "))
    # print definitions if existing
    for (no in unitno) {
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

# for a specific parent p (such as 'FE 2'), return a human-readable text of the summationGroup
printSumGroup <- function(summationGroups, mappingData, p, signofdiff, width) {
  childs <- summationGroups$child[summationGroups$parent %in% p]
  pn <- gsub(" [1-9]$", "", p)
  punit <- if (is.null(mappingData)) NULL else unique(mappingData$unit[mappingData$variable %in% pn])
  piamchilds <- if (is.null(mappingData)) NULL else sumNamesWithFactors(mappingData, pn)
  text <- paste0("\n", str_pad(paste(p, signofdiff, "  "), width + 5, "right", pad = "."), "   ",
                 paste0(piamchilds, " ", signofdiff)[! is.null(piamchilds)])
  chfactor <- summationGroups$factor[summationGroups$parent %in% p]
  chfactor <- paste0(ifelse(chfactor > 0, "+", "-"), ifelse(abs(chfactor) != 1, paste0(" ", abs(chfactor)), ""))
  for (ch in seq_along(childs)) {
    chunit <- NULL
    if (! is.null(mappingData)) {
      chunit <- unique(mappingData$unit[mappingData$variable %in% childs[[ch]]])
      chunit <- if (identical(chunit, punit) || length(chunit) == 0) NULL else paste0(" (", chunit, ")")
    }
    textch <- str_pad(paste0("  ", chfactor[[ch]], " ", childs[[ch]], chunit, " "), width + 5, "right", pad = ".")
    if (! is.null(mappingData)) {
      piamch <- sumNamesWithFactors(mappingData, childs[[ch]])
      if (! chfactor[[ch]] == "+" && ! as.character(piamch) %in% c("", "NA")) {
        piamch <- paste0(chfactor[[ch]], " (", gsub("^\\+ ", "", piamch), ")")
      }
      textch <- paste0(textch, "     ", piamch)
    }
    text <- c(text, textch)
  }
  return(text)
}
