#' Generates an IIASA submission from REMIND runs by applying a project-specific mapping
#'
#' @md
#' @author Falk Benke
#' @param mifDirectory path to directory with mif files of a REMIND run
#' @param model name of model registered with IIASA
#' @param mapping path to mapping applied
#' @param removeFromScen string to be removed from scenario name (optional)
#' @param addToScen string to be added as prefix to scenario name (optional)
#' @param outputDirectory path to directory for the generated submission (default: output)
#' @param outputPrefix gets prepended to the file name of the generated file(s)
#' @param logFile path to the logfile with warnings (default: output/missing.log)
#' @param generateSingleOutput indicates whether the submission should be generated
#'        into a single output file
#' @param outputFilename filename of the generated submission file,
#'        if `generateSingleOutput` is set to TRUE (default: submission.mif)
#' @importFrom data.table :=
#' @importFrom iamc write.reportProject
#' @importFrom rmndt readMIF writeMIF
#' @export
#'
generateIIASASubmission <- function(mifDirectory, model, mapping,
                          removeFromScen = NULL, addToScen = NULL,
                          outputDirectory = "output", outputPrefix = "",
                          logFile = "output/missing.log",
                          generateSingleOutput = FALSE, outputFilename = "submission.mif") {
  if (!dir.exists(outputDirectory)) {
    dir.create(outputDirectory)
  }

  .setModelAndScenario <- function(mif, model, scenRemove = NULL, scenAdd = NULL) {
    dt <- readMIF(mif)
    dt[, "Model" := model]
    if (!is.null(scenRemove)) dt[, "Scenario" := gsub(scenRemove, "", get("Scenario"))]
    if (!is.null(scenAdd)) {
      if (grepl(scenAdd, unique(dt$Scenario), fixed = TRUE)) {
        print(sprintf("Prefix %s already found in scenario name in %s.", scenAdd, mif))
      } else {
        dt[, "Scenario" := paste0(scenAdd, get("Scenario"))]
      }
    }
    writeMIF(dt, mif)
  }


  flist <- list.files(mifDirectory, "*.mif")
  for (fl in flist) {
    flPath <- file.path(mifDirectory, fl)
    .setModelAndScenario(
      flPath, model, removeFromScen, addToScen
    )
    if (generateSingleOutput) {
      iamc::write.reportProject(
        flPath, mapping,
        file.path(outputDirectory, outputFilename),
        append = TRUE,
        missing_log = logFile
      )
    } else {
      iamc::write.reportProject(
        flPath, mapping,
        file.path(outputDirectory, paste0(outputPrefix, fl)),
        missing_log = logFile
      )
    }
  }
}
