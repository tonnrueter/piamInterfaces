#' Model intercomparison plot
#'
#' @author Oliver Richters
#' @param mifFile path to the mif or xlsx file to apply summation checks to, or quitte object
#' @param outputDirectory path to directory to place logFile and dataDumpFile
#' @param summationsFile in inst/summations folder that describes the required summation groups
#' @param renameModels vector with oldname = newname
#' @param lineplotVariables vector with variable names for additional lineplots
#' @importFrom dplyr group_by summarise ungroup left_join mutate arrange %>% filter select desc
#' @importFrom rlang sym syms
#' @importFrom quitte as.quitte getModels getRegs getScenarios
#' @importFrom grDevices pdf dev.off
#' @examples
#'
#' plotIntercomparison(quitte::quitte_example_dataAR6,
#'                     lineplotVariables = c("Temperature|Global Mean", "Population"))
#'
#' @export
plotIntercomparison <- function(mifFile, outputDirectory = ".", summationsFile = "AR6", renameModels = NULL,
                                lineplotVariables = c("Temperature|Global Mean")) {
  .data <- NULL
  if (!is.null(outputDirectory) && !dir.exists(outputDirectory)) {
    dir.create(outputDirectory, recursive = TRUE)
  }

  summationGroups <- piamInterfaces::getSummations(summationsFile)
  if (summationsFile %in% names(piamInterfaces::summationsNames())) {
    summationsFile <- gsub(".*piamInterfaces", "piamInterfaces", piamInterfaces::summationsNames(summationsFile))
  }

  # load data and filter part that is in summation file
  data <- quitte::as.quitte(mifFile, na.rm = TRUE) %>%
    filter(!!sym("variable") %in% unique(c(summationGroups$child, summationGroups$parent, lineplotVariables))) %>%
    left_join(summationGroups, by = c("variable" = "child"))
  # use only regions that exist in all models
  regs <- getRegs(data)
  for (thismodel in quitte::getModels(data)) {
    regscen <- getRegs(filter(data, .data$model == thismodel) %>% droplevels())
    regs <- intersect(regs, regscen)
  }
  data <- filter(data, .data$region %in% regs) %>% droplevels()
  message("The filtered data contains ", length(unique(data$variable)), " variables.")
  if (! is.null(renameModels)) {
    newModelNames <- levels(data$model)
    for (n in names(renameModels)) {
      newModelNames <- gsub(n, renameModels[n], newModelNames, fixed = TRUE)
    }
    levels(data$model) <- newModelNames
  }

  checkVariables <- list()
  for (i in intersect(unique(summationGroups$parent), unique(data$variable))) {
    checkVariables[[i]] <- summationGroups[which(summationGroups[, "parent"] == i), "child"]
  }

  names(checkVariables) <- gsub(" [1-9]$", "", names(checkVariables))

  message("### ", length(quitte::getScenarios(data)), " documents will be generated, each with at most ",
          length(checkVariables), " area plots + ", length(lineplotVariables), " line plots. Enjoy waiting.\n")
  for (thisscenario in quitte::getScenarios(data)) {
    pdfFilename <- file.path(outputDirectory, paste0("model_comparison_", gsub(" ", "_", thisscenario), ".pdf"))
    pdf(pdfFilename,
        width = max(12, length(quitte::getRegs(data)), length(quitte::getModels(data)) * 2))
    message("\n## Writing ", pdfFilename, " for scenario '", thisscenario, "'.\n")
    plotdata <- filter(data, !!sym("scenario") == thisscenario) %>% droplevels()
    plotvariables <- sort(intersect(names(checkVariables), unique(plotdata$variable)))
    for (p in plotvariables) {
      childs <- intersect(checkVariables[[p]], unique(plotdata$variable))
      if (length(getModels(droplevels(filter(plotdata, .data$variable %in% childs)))) > 1) {
        message(which(p == plotvariables), "/", length(plotvariables), ": Add area plot for ",
                p, " with childs ", paste(gsub(p, "", childs, fixed = TRUE), collapse = ", "))
        mip::showAreaAndBarPlots(plotdata, if (length(childs) > 0) childs else p, tot = p, mainReg = "World",
                                 yearsBarPlot = c(2030, 2050), scales = "fixed")
      } else {
        message("For ", p, ", only one model has data. Skipping.")
      }
    }
    actualLineplots <- intersect(lineplotVariables, unique(plotdata$variable))
    for (v in actualLineplots) {
      message(which(v == actualLineplots), "/", length(actualLineplots), ": Add line plot for ", v)
      mip::showLinePlots(plotdata, v, mainReg = "World")
    }
    dev.off()
  }
  message("Done. See results in ", normalizePath(outputDirectory), ".")
}
