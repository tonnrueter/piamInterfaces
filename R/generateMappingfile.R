#' Generate mapping from the template containing all the metadata
#'
#' @md
#' @author Falk Benke
#' @param templates either a character or vector of templates to use as a starting point for creating the mapping
#' @param fileName name of the mapping file to be created, if provided, the file is created in the "output" folder,
#'        otherwise a data frame is returned (optional)
#' @param remindVar column name in template containing the REMIND variable names
#' @param remindUnit column name in template containing the REMIND units
#' @param targetVar column name in template containing the target variable names
#' @param targetUnit column name in template containing the target units
#' @param factorCol column name in template containing conversion factors
#' @param weightCol column name in template containing weight factors
#' @param commentFileName name of the comments file to be created, if provided,
#'        the file is created in the "output" folder, otherwise a data frame is
#'        returned (optional)
#' @param model exact name of the source model, used as column in comments file (optional)
#' @param spatialCol column name in template containing regional restrictions
#'        for reporting the corresponding variable (optional)
#'
#' @importFrom data.table fread fwrite := setnames
#' @export
#'
#'
generateMappingfile <- function(templates, fileName = NULL, remindVar, remindUnit,
                                targetVar, targetUnit, factorCol, weightCol,
                                spatialCol = "spatial", model = "REMIND-MAgPIE",
                                commentFileName = NULL) {
  if (!dir.exists("output")) {
    dir.create("output", recursive = TRUE)
  }

  .generateMapping <- function(template, fileName = NULL,
                               remindVar, remindUnit,
                               targetVar, targetUnit,
                               factorCol, weightCol,
                               spatialCol) {
    dt <- fread(template, sep = ";", )

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
      fwrite(dt, file = paste0("output/", fileName), sep = ";")
    } else {
      return(dt)
    }
  }

  .storeComments <- function(template, remindVar, targetVar, model, fileName = NULL) {
    dt <- fread(template)
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
        fwrite(comments, file = paste0("output/", fileName))
      } else {
        return(comments)
      }
    }
  }


  if (length(templates) == 1) {
    .generateMapping(
      template = templates,
      fileName = fileName,
      remindVar = remindVar,
      remindUnit = remindUnit,
      targetVar = targetVar,
      targetUnit = targetUnit,
      factorCol = factorCol,
      weightCol = weightCol,
      spatialCol = spatialCol
    )

    .storeComments(
      template = templates,
      remindVar = remindVar,
      targetVar = targetVar,
      model = model,
      fileName = commentFileName
    )
  } else {
    tmp <- NULL
    tmpComments <- NULL
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
      tmp <- rbind(tmp, m)

      c <- .storeComments(
        template = templates[i],
        remindVar = remindVar,
        model = model,
        targetVar = targetVar[i]
      )
      tmpComments <- rbind(tmpComments, c)
    }

    fwrite(tmp, file = paste0("output/", fileName), sep = ";")
    if (!is.null(tmpComments)) {
      fwrite(tmpComments, file = paste0("output/", commentFileName), sep = ";")
    }
  }
}
