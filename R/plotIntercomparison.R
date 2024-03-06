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
#' @param plotby whether you would like to have everything plotted by scenario, model and/or onefile
#' @param diffto if specified, the difference to this scenario is calculated and plotted
#' @importFrom dplyr group_by summarise ungroup left_join mutate arrange %>% filter select desc
#' @importFrom rlang sym syms .data
#' @importFrom quitte as.quitte getModels getRegs getScenarios
#' @importFrom grDevices pdf dev.off
#' @importFrom mip plotstyle plotstyle.add
#' @importFrom stats runif
#' @examples
#'
#' \dontrun{
#' plotIntercomparison(quitte::quitte_example_dataAR6,
#'                     lineplotVariables = c("Temperature|Global Mean", "Population"))
#' }
#'
#' @export
plotIntercomparison <- function(mifFile, outputDirectory = "output", summationsFile = "AR6", # nolint: cyclocomp_linter.
                                renameModels = NULL, lineplotVariables = "AR6", interactive = FALSE,
                                mainReg = "World", plotby = c("model", "scenario"), diffto = NULL) {

  if (!is.null(outputDirectory) && ! dir.exists(outputDirectory)) {
    dir.create(outputDirectory, recursive = TRUE)
  }

  summationGroups <- data.frame(list(variable = "", child = ""))
  if (! is.null(summationsFile)) {
    summationGroups <- getSummations(summationsFile)
    if (isTRUE(summationsFile %in% names(summationsNames()))) {
      summationsFile <- gsub(".*piamInterfaces", "piamInterfaces", summationsNames(summationsFile))
    }
  }

  templatefiles <- lineplotVariables[lineplotVariables %in% names(templateNames()) || file.exists(lineplotVariables)]
  tmpLpv <- setdiff(lineplotVariables, templatefiles)
  for (template in templatefiles) {
    tmpLpv <- c(tmpLpv, getTemplate(template)$Variable)
  }
  lineplotVariables <- unique(tmpLpv)

  # load data and filter part that is in summation file
  data <- quitte::as.quitte(mifFile, na.rm = TRUE) %>%
    filter(.data$variable %in% unique(c(summationGroups$child, summationGroups$parent, lineplotVariables))) %>%
    left_join(summationGroups, by = c("variable" = "child")) %>%
    droplevels()
  if (interactive) {
    data <- quitte::chooseFilter(data, types = c("model", "scenario", "region", "period"),
                                 keep = list(region = mainReg))
    plotby <- gms::chooseFromList(c("onefile", "model", "scenario"), "pdfs to be generated",
                          userinfo = "all in one file, and/or one file per model, scenario")
  }

  if (! is.null(diffto)) {
    stopifnot(
      `diffto has to have length 1` = length(diffto) == 1,
      `diffto has to be a scenario name present in the data` = diffto %in% levels(data$scenario)
    )
    dref <- data %>%
      filter(.data$scenario == diffto) %>%
      select(-"scenario") %>%
      rename("ref" = "value") %>%
      droplevels()
    data <- data %>%
      left_join(dref, by = c("model", "region", "variable", "unit", "period")) %>%
      mutate("value" = !!sym("value") - !!sym("ref")) %>%
      filter(.data$scenario != diffto) %>%
      droplevels()
  }

  message("The plots will show a total of ", length(unique(data$variable)), " variables.")
  newModelNames <- levels(data$model)
  for (n in names(renameModels)) {
    newModelNames <- gsub(n, renameModels[n], newModelNames, fixed = TRUE)
  }
  levels(data$model) <- newModelNames

  areaplotVariables <- list()
  for (i in intersect(unique(summationGroups$parent), unique(data$variable))) {
    childVariables <- intersect(summationGroups[which(summationGroups[, "parent"] == i), "child"],
                                unique(data$variable))
    if (length(childVariables) > 0) {
      areaplotVariables[[i]] <- childVariables
    }
  }
  names(areaplotVariables) <- gsub(" [1-9]$", "", names(areaplotVariables))
  plotvariables <- sort(intersect(c(names(areaplotVariables), lineplotVariables), unique(data$variable)))
  if (interactive) {
    userplotvariables <- gms::chooseFromList(plotvariables, userinfo = "Enter to select all variables",
                                             type = "variables to be plotted")
    if (length(userplotvariables) > 0) plotvariables <- userplotvariables
  }

  ps <- mip::plotstyle(as.character(runif(length(levels(data$model)))))
  output <- try(mip::plotstyle.add(levels(data$model), levels(data$model), ps, replace = TRUE))
  ps <- mip::plotstyle(as.character(runif(length(levels(data$scenario)))))
  output <- try(mip::plotstyle.add(levels(data$scenario), levels(data$scenario), ps, replace = TRUE))
  if (inherits(output, "try-error")) message("Error running mip::plotstyle.add, you may have inconsistent coloring.")

  countpdfs <- length(c(if ("scenario" %in% plotby) quitte::getScenarios(data),
                        if ("model" %in% plotby) quitte::getModels(data),
                        if ("onefile" %in% plotby) "onefile"))

  message("### ", countpdfs, " documents will be generated, each with max. ",
          length(plotvariables), " plots. Enjoy waiting.\n")

  if ("scenario" %in% plotby && length(quitte::getModels(data)) > 1) {
    for (thisscenario in quitte::getScenarios(data)) {
      pdfFilename <- file.path(outputDirectory, paste0("compare_models_", gsub(" ", "_", thisscenario), ".pdf"))
      message("\n## Writing ", pdfFilename, " for scenario '", thisscenario, "'.\n")
      plotdata <- filter(data, .data$scenario == thisscenario) %>% droplevels()
      makepdf(pdfFilename, plotdata, plotvariables, areaplotVariables, mainReg, diffto)
    }
  }
  if ("model" %in% plotby && length(quitte::getScenarios(data)) > 1) {
    for (thismodel in quitte::getModels(data)) {
      pdfFilename <- file.path(outputDirectory, paste0("compare_scenarios_", gsub(" ", "_", thismodel), ".pdf"))
      message("\n## Writing ", pdfFilename, " for model '", thismodel, "'.\n")
      plotdata <- filter(data, .data$model == thismodel) %>% droplevels()
      makepdf(pdfFilename, plotdata, plotvariables, areaplotVariables, mainReg, diffto)
    }
  }
  if ("onefile" %in% plotby || (length(quitte::getModels(data)) == 1 && length(quitte::getScenarios(data)) == 1)) {
    pdfFilename <- file.path(outputDirectory, "compare_scenarios_all.pdf")
    message("\n## Writing ", pdfFilename, " with all data.\n")
    makepdf(pdfFilename, droplevels(data), plotvariables, areaplotVariables, mainReg, diffto)
  }
  message("Done. See results in ", normalizePath(outputDirectory), ".")
}

makepdf <- function(pdfFilename, plotdata, plotvariables, areaplotVariables, mainReg, diffto = NULL) {
  if (nrow(plotdata) == 0) {
    message("plotdata empty, skipping.")
    return()
  }
  plotdata$identifier <- mip::identifierModelScen(plotdata)
  legendTitle <- paste(c(attr(plotdata$identifier, "deletedinfo"), "Model output")[[1]],
                       if (! is.null(diffto)) paste("difference to", diffto))
  if (! all(levels(plotdata$identifier) %in% names(mip::plotstyle()))) {
    ps <- mip::plotstyle(as.character(runif(length(levels(plotdata$identifier)))))
    try(mip::plotstyle.add(levels(plotdata$identifier), levels(plotdata$identifier), ps, replace = TRUE))
  }
  pdf(pdfFilename,
      width = 1.1 * max(12, length(quitte::getRegs(plotdata)),
                  length(quitte::getModels(plotdata)) * length(quitte::getScenarios(plotdata)) * 2),
      height = 1.1 * 7)
  plotvariables <- sort(intersect(plotvariables, unique(plotdata$variable)))
  for (p in plotvariables) {
    message(which(p == plotvariables), "/", length(plotvariables), ": Add plot for ", p)
    if (p %in% names(areaplotVariables)) {
      childs <- intersect(areaplotVariables[[p]], unique(plotdata$variable))
      if (length(getModels(droplevels(filter(plotdata, .data$variable %in% childs)))) > 1 ||
          length(getScenarios(droplevels(filter(plotdata, .data$variable %in% childs)))) > 1) {
        message("Childs: ", paste(gsub(p, "", childs, fixed = TRUE), collapse = ", "))
        mip::showAreaAndBarPlots(plotdata, if (length(childs) > 0) childs else p, tot = p, mainReg = mainReg,
                                 yearsBarPlot = c(2030, 2050), scales = "fixed")
        next
      }
    }
    # if no childs exist
    mip::showLinePlots(plotdata, p, mainReg = mainReg, color.dim.name = legendTitle)
  }
  dev.off()
  message("\n## Writing ", pdfFilename, " successful.\n")
}
