#' Generates an IIASA submission from REMIND runs by applying a project-specific mapping
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param mifs path to mif files or directories with mif files of a REMIND run
#' @param model name of model registered with IIASA
#' @param mapping mapping template names such as c("AR6", "AR6_NGFS"). If NULL, user is asked
#' @param mappingFile path to mapping. If NULL, mapping is generated based on param mapping
#' @param removeFromScen string to be removed from scenario name (optional)
#' @param addToScen string to be added as prefix to scenario name (optional)
#' @param outputDirectory path to directory for the generated submission (default: output)
#' @param outputPrefix gets prepended to the file name of the generated file(s)
#' @param logFile path to the logfile with warnings (default: output/missing.log)
#' @param generateSingleOutput indicates whether the submission should be generated
#'        into a single output file
#' @param outputFilename filename of the generated submission file,
#'        if `generateSingleOutput` is set to TRUE (default: submission.mif)
#' @param iiasatemplate optional filename of xlsx or yaml file provided by IIASA
#'        used to delete superfluous variables and adapt units
#' @param generatePlots boolean, whether to generate plots of failing summation checks
#' @param timesteps timesteps that are accepted in final submission
#' @importFrom data.table :=
#' @importFrom iamc write.reportProject
#' @importFrom magclass getNames getNames<- mbind read.report write.report
#' @importFrom quitte write.mif read.quitte write.IAMCxlsx
#' @importFrom stringr str_sub
#' @importFrom tidyr pivot_wider
#' @examples
#' \dontrun{
#' # Simple use. Generates submission file in output folder:
#' generateIIASASubmission(
#'   mifs = "/path/to/REMIMD/mifs",
#'   model = "REMIND-MAgPIE 2.1-4.2",
#'   mappingFile = "output/template_navigate.csv",
#'   generateSingleOutput = TRUE
#' )
#' }
#' @export
generateIIASASubmission <- function(mifs = ".", mapping = NULL, model = "REMIND 3.0", # nolint: cyclocomp_linter.
                                    mappingFile = NULL,
                                    removeFromScen = NULL, addToScen = NULL,
                                    outputDirectory = "output", outputPrefix = "",
                                    logFile = "output/missing.log",
                                    generateSingleOutput = TRUE,
                                    outputFilename = "submission.mif",
                                    iiasatemplate = NULL, generatePlots = FALSE,
                                    timesteps = c(seq(2005, 2060, 5), seq(2070, 2100, 10))) {

  if (isTRUE(timesteps == "all")) timesteps <- seq(1, 3000)
  dir.create(outputDirectory, showWarnings = FALSE)

  # for each directory, include all mif files
  flist <- unique(c(mifs[!dir.exists(mifs)], list.files(mifs[dir.exists(mifs)], "*.mif", full.names = TRUE)))

  # generate mapping file, if they don't yet exist
  if (length(mapping) > 0 || is.null(mappingFile) || !file.exists(mappingFile)) {
    if (is.null(mappingFile)) {
      mappingFile <- file.path(outputDirectory, paste0(paste0(c("mapping", mapping), collapse = "_"), ".csv"))
    }
    invisible(generateMappingfile(templates = mapping, outputDirectory = NULL,
                              fileName = mappingFile, model = model, logFile = logFile,
                              iiasatemplate = iiasatemplate))
  }

  message("\n### Generating .mif and .xlsx files using mapping ", mappingFile, ".")
  if (!is.null(model)) message("# Correct model name to '", model, "'.")
  message("# Adapt scenario names: '",
          addToScen, "' will be prepended, '", removeFromScen, "' will be removed.")
  message("# Apply mapping from ", mappingFile)

  tmpfile <- file.path(outputDirectory, "tmp_reporting.mif")
  outputMif <- file.path(outputDirectory, paste0(gsub("\\.mif$|\\.xlsx$", "", outputFilename), ".mif"))

  allmifdata <- NULL
  for (fl in seq_along(flist)) {
    if (!generateSingleOutput) {
      outputMif <- file.path(outputDirectory, paste0(outputPrefix, basename(flist[fl])))
      allmifdata <- NULL
    }
    message("# read ", flist[fl])
    mifdata <- quitte::as.quitte(flist[fl])
    # remove -rem-xx and mag-xx from scenario names
    mifdata$scenario <- gsub("-(rem|mag)-[0-9]{1,2}", "", mifdata$scenario)
    allmifdata <- rbind(allmifdata, mifdata)
    if (!generateSingleOutput || fl == length(flist)) {
      message("# Convert to ", outputMif)
      mifdata <- .setModelAndScenario(allmifdata, model, removeFromScen, addToScen)
      quitte::write.mif(mifdata, tmpfile)
      iamc::write.reportProject(
          tmpfile,
          mappingFile,
          file = tmpfile,
          missing_log = logFile
      )
      message("# Restore PM2.5 dot in variable names for consistency with DB template")
      mifdata <- quitte::as.quitte(tmpfile) %>% mutate(variable = gsub("PM2_5", "PM2.5", !!sym("variable")))

      message("# Replace N/A for missing years with blanks as recommended by Ed Byers")
      write.mif(mifdata %>% mutate(value = ifelse(is.na(!!sym("value")), "", !!sym("value"))), outputMif)

      mifdata <- mifdata %>%
        mutate(value = ifelse(!is.finite(!!sym("value")) | is.na(!!sym("value")), NA, !!sym("value"))) %>%
        mutate(scenario = as.factor(gsub("^NA$", "", !!sym("scenario")))) %>%
        filter(!!sym("period") %in% timesteps)

      if (!is.null(iiasatemplate) && file.exists(iiasatemplate)) {
        mifdata <- checkIIASASubmission(mifdata, iiasatemplate, logFile)
      } else {
        message("# iiasatemplate ", iiasatemplate, " does not exist, returning full list of variables.")
      }

      # check whether all scenarios have same number of variables
      countDataPoints <- seq_along(levels(mifdata$scenario))
      for (i in countDataPoints) {
        countDataPoints[i] <- sum(mifdata$scenario == levels(mifdata$scenario)[[i]])
      }
      if (length(unique(countDataPoints)) != 1) {
        message(
          "Not all scenarios have the same data points: ",
          paste0(levels(mifdata$scenario), ": ", countDataPoints, ".", collapse = " ")
        )
      }

      message("# In mif file, replace N/A for missing years with blanks as recommended by Ed Byers")
      write.mif(mifdata %>% mutate(value = ifelse(is.na(!!sym("value")), "", !!sym("value"))), outputMif)

      # perform summation checks
      for (sumFile in intersect(mapping, names(summationsNames()))) {
        invisible(checkSummations(mifdata, template = mappingFile, summationsFile = sumFile,
                                logFile = basename(logFile), logAppend = TRUE, outputDirectory = outputDirectory,
                                generatePlots = generatePlots))
      }

      outputXlsx <- paste0(gsub("\\.mif$", "", outputMif), ".xlsx")
      quitte::write.IAMCxlsx(mifdata, outputXlsx)
      message("\n### Output files written:\n- ", outputMif, "\n- ", outputXlsx, "\n")
    }
  }
}

.setModelAndScenario <- function(mif, modelname, scenRemove = NULL, scenAdd = NULL) {
    dt <- quitte::as.quitte(mif)
    scenarioNames <- unique(dt$scenario)
    if (!is.null(modelname)) dt$model <- modelname
    if (!is.null(scenRemove)) dt$scenario <- gsub(scenRemove, "", dt$scenario)
    if (!is.null(scenAdd)) {
      if (all(grepl(scenAdd, unique(dt$scenario), fixed = TRUE))) {
        message(sprintf("Prefix %s already found in all scenario name in %s. Skipping.", scenAdd, mif))
      } else {
        dt$scenario <- paste0(scenAdd, dt$scenario)
      }
    }
    if (length(unique(dt$scenario)) < length(scenarioNames)) {
      message(length(scenarioNames), " scenario names before changes: ", paste(scenarioNames, collapse = ", "))
      message(length(unique(dt$scenario)), " scenario names after changes:  ",
              paste(unique(dt$scenario), collapse = ", "))
      stop("Changes to scenario names lead to duplicates. Adapt scenRemove='",
           scenRemove, "' and scenAdd='", scenAdd, "'!")
    }
    return(dt)
}
