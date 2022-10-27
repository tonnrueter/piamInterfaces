#' Generate mapping from the template containing all the metadata
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param templates either a character or vector of project templates to use as a starting point for
#'        creating the mapping, valid projects are "AR6", "SHAPE", "NAVIGATE"
#' @param outputDirectory path to directory to place generated files (default: output)
#' @param fileName name of the mapping file to be created, if provided, the file is created in the "output" folder,
#'        otherwise a data frame is returned (optional)
#' @param logFile name of the comments file to be created, if provided,
#'        the file is created in the "output" folder, otherwise a data frame is
#'        returned (optional)
#' @param iiasatemplate filename of xlsx or yaml file provided by IIASA
#' @param targetVar (vector of) column name in template(s) containing the target variable names
#'        (default: variable)
#' @param targetUnit column name in template containing the target units (default: unit)
#' @param remindVar column name in template containing the REMIND variable names (default: piam_variable)
#' @param remindUnit column name in template containing the REMIND units (default: piam_unit)
#' @param factorCol column name in template containing conversion factors (default: piam_factor)
#' @param weightCol column name in template containing weight factors (optional)
#' @param spatialCol column name in template containing regional restrictions
#'        for reporting the corresponding variable (optional)
#' @param model exact name of the source model, used as column in comments file (default: REMIND-MAgPIE)
#' @importFrom data.table as.data.table fread fwrite := setnames
#' @examples
#' \dontrun{
#' # Simple use. Creates NAVIGATE mapping and saves it to /output/template_navigate.csv:
#' generateMappingfile(
#'   templates = "NAVIGATE",
#'   fileName = "template_navigate.csv"
#' )
#' # More complex use. Creates combined mapping from NAVIGATE and SHAPE template:
#' generateMappingfile(
#'   templates = c("NAVIGATE", "SHAPE"),
#'   fileName = "template_combined.csv"
#' )
#' }
#' @export
generateMappingfile <- function(templates = NULL, outputDirectory = "output",
                                fileName = NULL, logFile = NULL,
                                iiasatemplate = NULL,
                                targetVar = "variable", targetUnit = "unit",
                                remindVar = "piam_variable", remindUnit = "piam_unit",
                                factorCol = "piam_factor", weightCol = NULL, spatialCol = "spatial",
                                model = "REMIND-MAgPIE"
                                ) {
  if (is.null(templates)) {
    templates <- chooseFromList(names(templateNames()), type = "templates",
                                returnBoolean = FALSE, multiple = TRUE, addAllPattern = FALSE)
    if (length(templates) == 0) stop("No template selected, abort.")
  }
  for (v in c("targetVar", "targetUnit", "remindVar", "remindUnit", "factorCol", "weightCol", "spatialCol")) {
    assign(v, tolower(get(v)))
  }
  if (!is.null(outputDirectory) && !dir.exists(outputDirectory)) {
    dir.create(outputDirectory, recursive = TRUE)
  }
  message("### Generating mapping file based on templates ", paste(templates, collapse = ", "))
  if (length(targetVar) == 1 && length(templates) > 1) targetVar <- rep.int(targetVar, length(templates))
  mapping <- NULL
  comments <- NULL

  for (i in seq_along(templates)) {
    dt <- as.data.table(getTemplate(templates[i]))
    names(dt) <- tolower(names(dt))

    ## remove to dos and empty mappings
    dt[get(remindVar) == "TODO", (remindVar) := ""]
    dt <- dt[get(remindVar) != ""]

    ## store comments
    com <- dt[get("comment") != ""]
    if (nrow(com) > 0) {
      com <- com[, list("Model" = model, "Scenario" = "All", "Region" = "All", "Variable" = get(targetVar[i]),
                        "Year" = "All", "Comment" = get("comment"))]
      comments <- rbind(comments, com)
    }

    ## factor defaults to 1
    dt[is.na(get(factorCol)), (factorCol) := 1]

    # remove empty spatial column, as it is interpreted as logical column
    if (spatialCol %in% colnames(dt) && typeof(dt[, get(spatialCol)]) == "logical") {
      dt[, (spatialCol) := NULL]
    }
    # spatial defaults to "reg+glo"
    if (spatialCol %in% colnames(dt)) {
      dt[get(spatialCol) == "", (spatialCol) := "reg+glo"]
    } else {
      dt[, (spatialCol) := "reg+glo"]
    }

    noUnit <- dt[get(remindUnit) == ""]
    if (nrow(noUnit)) {
      warning(sprintf("No unit found for variables %s", paste(noUnit[[remindVar]], collapse = ", ")))
    }

    ## remove variables not in iiasatemplate and correct units
    if (! is.null(iiasatemplate)) {
      dt <- rbind(checkIIASASubmission(dt, iiasatemplate, logFile = logFile, failOnUnitMismatch = FALSE))
    }

    dt <- dt[, c(remindVar, targetVar[i], "factor", "weight", "spatial") :=
      list(
        sprintf("%s (%s)", get(remindVar), get(remindUnit)),
        sprintf("%s (%s)", get(targetVar[i]), get(targetUnit)),
        get(factorCol),
        ifelse(length(weightCol) == 0, "NULL", get(weightCol)),
        get(spatialCol)
      )][
      , c(remindVar, targetVar[i], "factor", "weight", "spatial"),
      with = FALSE
    ]
    setnames(dt, tolower(targetVar[i]), "Variable")
    mapping <- rbind(mapping, dt)
  }

  if (!is.null(fileName)) {
    fwrite(mapping, file = paste0(outputDirectory, "/", fileName), sep = ";")
  }
  if (!is.null(comments) && !is.null(logFile)) {
    fwrite(comments, file = paste0(outputDirectory, "/", logFile), sep = ";")
  }
  return(invisible(list(mappings = mapping, comments = comments)))
}
