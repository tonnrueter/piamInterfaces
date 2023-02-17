#' Model intercomparison plots: area plots based on summation groups, line plots for further
#' variables. Creates a PDF for each model and scenario in the outputDirectory
#'
#' @author Oliver Richters
#' @param mifFile path to the mif or xlsx file to apply summation checks to, or quitte object
#' @param outputDirectory path to directory to place one PDF for each model and scenario
#' @param summationsFile in inst/summations folder that describes the required summation groups
#' @param renameModels vector with oldname = newname
#' @param lineplotVariables vector with variable names for additional lineplots or filenames
#'        of files containing a 'Variable' column (or both)
#' @param interactive boolean whether you want to select variables, regions and models to be plotted
#' @param mainReg region name of main region to be passed to mip
#' @importFrom dplyr group_by summarise ungroup left_join mutate arrange %>% filter select desc
#' @importFrom rlang sym syms
#' @importFrom quitte as.quitte getModels getRegs getScenarios
#' @importFrom grDevices pdf dev.off
#' @examples
#'
#' \dontrun{
#' plotIntercomparison(quitte::quitte_example_dataAR6,
#'                     lineplotVariables = c("Temperature|Global Mean", "Population"))
#' }
#'
#' @export
plotIntercomparison <- function(mifFile, outputDirectory = ".", summationsFile = "AR6", # nolint: cyclocomp_linter.
                                renameModels = NULL, lineplotVariables = "AR6", interactive = FALSE,
                                mainReg = "World") {
  .data <- NULL # avoid binding lintr error

  if (!is.null(outputDirectory) && ! dir.exists(outputDirectory)) {
    dir.create(outputDirectory, recursive = TRUE)
  }

  summationGroups <- getSummations(summationsFile)
  if (summationsFile %in% names(summationsNames())) {
    summationsFile <- gsub(".*piamInterfaces", "piamInterfaces", summationsNames(summationsFile))
  }

  templatefiles <- lineplotVariables[lineplotVariables %in% names(templateNames()) || file.exists(lineplotVariables)]
  tmpLpv <- setdiff(lineplotVariables, templatefiles)
  for (template in templatefiles) {
    templatedata <- getTemplate(template)
    tmpLpv <- c(tmpLpv, setdiff(templatedata$Variable, c(summationGroups$parent, summationGroups$child)))
  }
  lineplotVariables <- unique(tmpLpv)

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
  if (interactive) {
    regs <- c(mainReg, gms::chooseFromList(setdiff(regs, mainReg), type = "regions to be plotted"))
    models <- gms::chooseFromList(levels(data$model), type = "models to be plotted")
    scenarios <- gms::chooseFromList(levels(data$scenario), type = "scenarios to be plotted")
    data <- filter(data, .data$model %in% models & .data$scenario %in% scenarios) %>% droplevels()
  }
  data <- filter(data, .data$region %in% regs) %>% droplevels()

  message("The filtered data contains ", length(unique(data$variable)), " variables.")
  newModelNames <- levels(data$model)
  for (n in names(renameModels)) {
    newModelNames <- gsub(n, renameModels[n], newModelNames, fixed = TRUE)
  }
  levels(data$model) <- newModelNames

  # temporary fix until https://github.com/pik-piam/mip/issues/56 is fixed
  levels(data$model) <- gsub("\\)", "", gsub("\\(", "", levels(data$model)))
  levels(data$scenario) <- gsub("\\)", "", gsub("\\(", "", levels(data$scenario)))

  checkVariables <- list()
  for (i in intersect(unique(summationGroups$parent), unique(data$variable))) {
    checkVariables[[i]] <- summationGroups[which(summationGroups[, "parent"] == i), "child"]
  }

  names(checkVariables) <- gsub(" [1-9]$", "", names(checkVariables))

  message("### ", length(c(quitte::getScenarios(data), quitte::getModels(data))),
          " documents will be generated, each with at most ",
          length(checkVariables), " area plots + ", length(lineplotVariables), " line plots. Enjoy waiting.\n")

  makepdf <- function(pdfFilename, plotdata, plotvariables) {
    if (nrow(plotdata) == 0) {
      message("plotdata empty, skipping.")
      return()
    }
    pdf(pdfFilename,
        width = max(12, length(quitte::getRegs(plotdata)),
                    length(quitte::getModels(plotdata)) * length(quitte::getScenarios(plotdata)) * 2))
    plotvariables <- sort(intersect(plotvariables, unique(plotdata$variable)))
    for (p in plotvariables) {
      message(which(p == plotvariables), "/", length(plotvariables), ": Add plot for ", p)
      if (p %in% names(checkVariables)) {
        childs <- intersect(checkVariables[[p]], unique(plotdata$variable))
        if (length(getModels(droplevels(filter(plotdata, .data$variable %in% childs)))) > 1 ||
            length(getScenarios(droplevels(filter(plotdata, .data$variable %in% childs)))) > 1) {
          message("Childs: ", paste(gsub(p, "", childs, fixed = TRUE), collapse = ", "))
          mip::showAreaAndBarPlots(plotdata, if (length(childs) > 0) childs else p, tot = p, mainReg = mainReg,
                                   yearsBarPlot = c(2030, 2050), scales = "fixed")
          next
        }
      }
      # if no childs exist
      mip::showLinePlots(plotdata, p, mainReg = mainReg)
    }
    dev.off()
  }

  plotvariables <- sort(intersect(c(names(checkVariables), lineplotVariables), unique(data$variable)))
  if (interactive) plotvariables <- gms::chooseFromList(plotvariables, type = "variables to be plotted")

  if (length(quitte::getModels(data)) > 1) {
    for (thisscenario in quitte::getScenarios(data)) {
      pdfFilename <- file.path(outputDirectory, paste0("compare_models_", gsub(" ", "_", thisscenario), ".pdf"))
      message("\n## Writing ", pdfFilename, " for scenario '", thisscenario, "'.\n")
      plotdata <- filter(data, !!sym("scenario") == thisscenario) %>% droplevels()
      makepdf(pdfFilename, plotdata, plotvariables)
    }
  }
  if (length(quitte::getScenarios(data)) > 1 || length(quitte::getModels(data)) == 1) {
    for (thismodel in quitte::getModels(data)) {
      pdfFilename <- file.path(outputDirectory, paste0("compare_scenarios_", gsub(" ", "_", thismodel), ".pdf"))
      message("\n## Writing ", pdfFilename, " for model '", thismodel, "'.\n")
      plotdata <- filter(data, !!sym("model") == thismodel) %>% droplevels()
      makepdf(pdfFilename, plotdata, plotvariables)
    }
  }
  message("Done. See results in ", normalizePath(outputDirectory), ".")
}
