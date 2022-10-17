#' Generate mapping from the template containing all the metadata
#'
#' @md
#' @author Falk Benke
#' @param templates either a character or vector of project templates to use as a starting point for
#'        creating the mapping, valid projects are "AR6", "SHAPE", "NAVIGATE"
#' @param outputDirectory path to directory to place generated files (default: output)
#' @param fileName name of the mapping file to be created, if provided, the file is created in the "output" folder,
#'        otherwise a data frame is returned (optional)
#' @param commentFileName name of the comments file to be created, if provided,
#'        the file is created in the "output" folder, otherwise a data frame is
#'        returned (optional)
#' @param targetVar column name in template containing the target variable names
#' @param targetUnit column name in template containing the target units (default: Unit)
#' @param remindVar column name in template containing the REMIND variable names (default: r30m44)
#' @param remindUnit column name in template containing the REMIND units (default: r30m44_unit)
#' @param factorCol column name in template containing conversion factors (default: r30m44_factor)
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
                                fileName = NULL, commentFileName = NULL,
                                targetVar = "Variable", targetUnit = "Unit",
                                remindVar = "r30m44", remindUnit = "r30m44_unit",
                                factorCol = "r30m44_factor", weightCol = NULL, spatialCol = "spatial",
                                model = "REMIND-MAgPIE"
                                ) {
  if (is.null(templates)) {
    templates <- chooseFromList(names(templateNames()), type = "templates",
                                returnBoolean = FALSE, multiple = TRUE, addAllPattern = FALSE)
    if (length(templates) == 0) stop("No template selected, abort.")
  }
  if (!is.null(outputDirectory) && !dir.exists(outputDirectory)) {
    dir.create(outputDirectory, recursive = TRUE)
  }

  .generateMapping <- function(template, fileName = NULL,
                               remindVar, remindUnit,
                               targetVar, targetUnit,
                               factorCol, weightCol,
                               spatialCol) {
    dt <- as.data.table(getTemplate(template))

    ## remove to dos and empty mappings
    dt[get(remindVar) == "TODO", (remindVar) := ""]
    dt <- dt[get(remindVar) != ""]
    ## factor defaults to 1
    dt[is.na(get(factorCol)), (factorCol) := 1]

    ## spatial defaults to "reg+glo"

    # remove empty spatial column, as it is interpreted as logical column
    if (spatialCol %in% colnames(dt) && typeof(dt[, get(spatialCol)]) == "logical") {
      dt[, (spatialCol) := NULL]
    }

    if (spatialCol %in% colnames(dt)) {
      dt[get(spatialCol) == "", (spatialCol) := "reg+glo"]
    } else {
      dt[, (spatialCol) := "reg+glo"]
    }

    noUnit <- dt[get(remindUnit) == ""]
    if (nrow(noUnit)) {
      warning(sprintf("No unit found for variables %s", paste(noUnit[[remindVar]], collapse = ", ")))
    }

    dt <- dt[, c(remindVar, targetVar, "factor", "weight", "spatial") :=
      list(
        sprintf("%s (%s)", get(remindVar), get(remindUnit)),
        sprintf("%s (%s)", get(targetVar), get(targetUnit)),
        get(factorCol),
        ifelse(is.null(weightCol), "NULL", get(weightCol)),
        get(spatialCol)
      )][
      , c(remindVar, targetVar, "factor", "weight", "spatial"),
      with = FALSE
    ]

    if (!is.null(fileName)) {
      ## store mapping
      fwrite(dt, file = paste0(outputDirectory, "/", fileName), sep = ";")
    }
    return(dt)
  }

  .storeComments <- function(template, remindVar, targetVar, model, fileName = NULL) {
    dt <- as.data.table(getTemplate(template))
    dt[get(remindVar) == "TODO", (remindVar) := ""]
    dt <- dt[get(remindVar) != ""]

    comments <- dt[get("Comment") != ""]

    if (nrow(comments) > 0) {
      comments <- comments[, list(
        "Model" = model,
        "Scenario" = "All",
        "Region" = "All",
        "Variable" = get(targetVar),
        "Year" = "All",
        "Comment" = get("Comment")
      )]

      if (!is.null(fileName)) {
        fwrite(comments, file = paste0(outputDirectory, "/", fileName))
      }
      return(comments)
    }
  }
  if (length(targetVar) == 1 && length(templates) > 1) targetVar <- rep.int(targetVar, length(templates))
  mapping <- NULL
  comments <- NULL
  for (i in seq_along(templates)) {
    m <- .generateMapping(
      template = templates[i],
      remindVar = remindVar,
      remindUnit = remindUnit,
      targetVar = targetVar[i],
      targetUnit = targetUnit,
      factorCol = factorCol,
      weightCol = weightCol,
      spatialCol = spatialCol
    )
    setnames(m, targetVar[i], "Variable")
    mapping <- rbind(mapping, m)

    c <- .storeComments(
      template = templates[i],
      remindVar = remindVar,
      model = model,
      targetVar = targetVar[i]
    )
    comments <- rbind(comments, c)

    if (!is.null(fileName)) {
      fwrite(mapping, file = paste0(outputDirectory, "/", fileName), sep = ";")
    }
    if (!is.null(comments) && !is.null(commentFileName)) {
      fwrite(comments, file = paste0(outputDirectory, "/", commentFileName), sep = ";")
    }
  }
  return(list(mappings = mapping, comments = comments))
}
