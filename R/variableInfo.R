#' Provide information on variable, its mappings and summation groups
#'
#' @md
#' @author Oliver Richters
#' @param varname string with variable name
#' @param template template shortcut (AR6, NAVIGATE). NULL means all
#' @param remindVar column name of variable in templates (default: piam_variable)
#' @param remindUnit column name of unit in templates (default: piam_unit)
#' @importFrom stringr str_split str_pad
#' @importFrom magclass unitsplit
#' @return prints human-readable summary to the user
#' @examples
#' # Simple use. prints human-readable summary to the reader on Emi|CO2:
#' variableInfo(
#'   "Emi|CO2"
#' )
#' @export
variableInfo <- function(varname, template = NULL, remindVar = "piam_variable", remindUnit = "piam_unit") {
  templates <- templateNames(template)

  green <- "\033[0;32m"
  blue  <- "\033[0;34m"
  nc    <- "\033[0m"   # No Color
  width <- 60

  .getChilds <- function(v, c) {
    c[which(grepl(paste0("^", gsub("|", "\\|", v, fixed = TRUE), "\\|[^\\|]*$"), gsub("+|", "", c, fixed = TRUE)))]
  }

  message("\n##### Search for information on ", green, varname, nc, " in mapping templates")
  for (t in names(templates)) {
    templateData <- getTemplate(t)
    template <- basename(templateNames(t))
    remindno <- which(varname == templateData[, remindVar])
    exportno <- which(varname == templateData$Variable)
    if (length(remindno) + length(exportno) == 0) {
      message("\n### Nothing found in template: ", blue, template, nc)
      next
    } else {
      message("\n### Results from template: ", blue, template, nc)
    }
    if (t %in% names(summationsNames())) {
      summationGroups <- getSummations(t)
      # print table column names
      summationsFile <- gsub(".*piamInterfaces", "piamInterfaces", summationsNames(t))
      message(str_pad(paste0("# Export variable groups found in ",
                           basename(summationsFile)), width + 8, "right"),
            "# Corresponding ", remindVar, " variables")
    } else {
      summationGroups <- NULL
      message("# No corresponding summation groups file found, show variables in mapping template.")
      message("\n", str_pad(paste0("# Export variable"), width + 8, "right"),
            "# Corresponding ", remindVar, " variables")
    }
    for (no in unique(c(remindno, exportno))) {
      exportname <- templateData$Variable[no]
      remindname <- templateData[no, remindVar]
      allexportchilds <- unique(.getChilds(exportname, templateData$Variable))
      allremindchilds <- unique(.getChilds(varname, templateData[, remindVar]))
      if (exportname %in% summationGroups$parent) {
        exportchilds <- summationGroups$child[summationGroups$parent == exportname]
        # print summation parents
        message(". ", str_pad(paste(exportname, "="), width + 3, "right"), "   . ",
                paste0(unitsplit(templateData[, remindVar][unitsplit(templateData$Variable)$variable
                                                           == exportname])$variable, collapse = " + "), " =")
        # print summation childs
        for (ch in exportchilds) {
          remindchilds <- unitsplit(templateData[, remindVar][unitsplit(templateData$Variable)$variable == ch])$variable
          message("  + ", str_pad(ch, width + 2, "right"), "    + ", paste0(remindchilds, collapse = " + "))
          allremindchilds <- setdiff(allremindchilds, remindchilds)
        }
        allexportchilds <- setdiff(allexportchilds, exportchilds)
      } else {
        message(". ", str_pad(paste0(exportname, " (", templateData$Unit[no], ")"), width + 3, "right"),
                "   . ", remindname, " (", templateData[, remindUnit][no], ")")
      }
      if (length(allexportchilds) + length(allremindchilds) > 0) {
        message("\n# Child variables", if (t %in% names(summationsNames())) " not in summation group")
        for (ch in allexportchilds) {
          remindchilds <- unitsplit(templateData[, remindVar][unitsplit(templateData$Variable)$variable == ch])$variable
          message("  . ", str_pad(ch, width + 1, "right"), "     . ", paste0(remindchilds, collapse = " + "))
          allremindchilds <- setdiff(allremindchilds, remindchilds)
        }
        for (ch in allremindchilds) {
          message("   . ", str_pad("NA", width, "right"), "     . ", ch)
        }
      }
      message("Units: ", str_pad(paste0(unique(templateData$Unit[no]), collapse = ", "), width, "right"),
              " Units: ", paste0(unique(templateData[, remindUnit][no]), collapse = ", "))
    }
  }
}
