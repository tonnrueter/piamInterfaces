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
#' @importFrom rmndt readMIF writeMIF
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

  tmpfile <- file.path(outputDirectory, "tmp_reporting.tmp")
  outputMif <- file.path(outputDirectory, paste0(gsub("\\.mif$|\\.xlsx$", "", outputFilename), ".mif"))

  allmifdata <- NULL
  for (fl in seq_along(flist)) {
    if (!generateSingleOutput) {
      outputMif <- file.path(outputDirectory, paste0(outputPrefix, basename(flist[fl])))
      allmifdata <- NULL
    }
    message("# read ", flist[fl])
    mifdata <- read.report(flist[fl], as.list = FALSE)
    # remove -rem-xx and mag-xx from scenario names
    getNames(mifdata, dim = 1) <- gsub("-(rem|mag)-[0-9]{1,2}", "", getNames(mifdata, dim = 1))
    allmifdata <- mbind(allmifdata, mifdata)
    if (!generateSingleOutput || fl == length(flist)) {
      message("# Write ", tmpfile)
      write.report(allmifdata, file = tmpfile)
      message("# Convert to ", outputMif)
      .setModelAndScenario(tmpfile, model, removeFromScen, addToScen)
      iamc::write.reportProject(
        tmpfile, mappingFile,
        file = outputMif,
        missing_log = logFile,
      )
      unlink(tmpfile)

      message("# Read data again")
      mifdata <- read.quitte(outputMif, factors = FALSE) %>%
        mutate(value = ifelse(!is.finite(!!sym("value")) | is.na(!!sym("value")), NA, !!sym("value"))) %>%
        mutate(scenario = gsub("^NA$", "", !!sym("scenario"))) %>%
        filter(!!sym("period") %in% timesteps)

      if (!is.null(iiasatemplate) && file.exists(iiasatemplate)) {
        mifdata <- checkIIASASubmission(mifdata, iiasatemplate, logFile)
      } else {
        message("# iiasatemplate ", iiasatemplate, " does not exist, returning full list of variables.")
      }

      # check whether all scenarios have same number of variables
      scenarios <- unique(mifdata$variable)
      for (i in seq_along(scenarios)) {
        if (length(filter(mifdata, !!sym("variable") %in% scenarios[[1]])) !=
            length(filter(mifdata, !!sym("variable") %in% scenarios[[i]]))) {
          warning(scenarios[1], " has a different number of variables than ", scenarios[i])
        }
      }

      unlink(outputMif)

      message("# Restore PM2.5 dot in variable names for consistency with DB template")
      mifdata <- mifdata %>% mutate(variable = gsub("PM2_5", "PM2.5", !!sym("variable")))

      message("# Replace N/A for missing years with blanks as recommended by Ed Byers")
      write.mif(mifdata %>% mutate(value = ifelse(is.na(!!sym("value")), "", !!sym("value"))), outputMif)

      # perform summation checks
      for (sumFile in intersect(mapping, names(summationsNames()))) {
        invisible(checkSummations(outputMif, template = mappingFile, summationsFile = sumFile,
                                logFile = basename(logFile), logAppend = TRUE, outputDirectory = outputDirectory,
                                generatePlots = generatePlots))
      }

      outputXlsx <- paste0(gsub("\\.mif$", "", outputMif), ".xlsx")
      quitte::write.IAMCxlsx(mifdata, outputXlsx)
      message("\n### Output files written:\n- ", outputMif, "\n- ", outputXlsx, "\n")
    }
  }
}

.setModelAndScenario <- function(mif, model, scenRemove = NULL, scenAdd = NULL) {
    Scenario <- NULL # nolint, added to avoid no visible binding error
    Model <- NULL    # nolint
    dt <- readMIF(mif)
    scenarioNames <- unique(dt$Scenario)
    if (!is.null(model)) dt[, Model := model]
    if (!is.null(scenRemove)) dt[, Scenario := gsub(scenRemove, "", Scenario)]
    if (!is.null(scenAdd)) {
      if (all(grepl(scenAdd, unique(dt$Scenario), fixed = TRUE))) {
        message(sprintf("Prefix %s already found in all scenario name in %s. Skipping.", scenAdd, mif))
      } else {
        dt[, Scenario := paste0(scenAdd, Scenario)]
      }
    }
    if (length(unique(dt$Scenario)) < length(scenarioNames)) {
      message(length(scenarioNames), " scenario names before changes: ", paste(scenarioNames, collapse = ", "))
      message(length(unique(dt$Scenario)), " scenario names after changes:  ",
              paste(unique(dt$Scenario), collapse = ", "))
      stop("Changes to scenario names lead to duplicates. Adapt scenRemove='",
           scenRemove, "' and scenAdd='", scenAdd, "'!")
    }
    writeMIF(dt, mif)
}
