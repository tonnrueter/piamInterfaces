#' Model intercomparison plots: area plots based on summation groups, line plots for further
#' variables. Creates a PDF for each model and scenario in the outputDirectory
#'
#' @author Oliver Richters
#' @param mifFile path to the mif or xlsx file to apply summation checks to, or quitte object
#' @param outputDirectory path to directory to place one PDF for each model and scenario
#' @param summationsFile in inst/summations folder that describes the required summation groups.
#'        set to "extractVariableGroups" to extract it automatically from data based on |+| notation
#' @param renameModels vector with oldname = newname
#' @param lineplotVariables vector with variable names for lineplots or filenames
#'        of files containing a 'variable' column (or both)
#' @param areaplotVariables vector with variable names for areaplot or filenames
#'        of files containing a 'variable' column. Only those available in the summationsFile can be plotted.
#' @param interactive allows to select various settings interactively:
#'        subset of c("variable", "model", "scenario", "region", "period", "plotby", "diffto", "yearsBarPlot")
#'        or set to TRUE to select all of them
#' @param mainReg region name of main region to be passed to mip
#' @param plotby whether you would like to have everything plotted by scenario, model and/or onefile.
#'        set to NULL to be asked.
#' @param diffto if specified, the difference to this scenario is calculated and plotted
#' @param yearsBarPlot years for which bar plots are to be made.
#' @param postfix to the filename, defaults to something like "_2024-09-05_12.47.28"
#' @importFrom dplyr group_by summarise ungroup left_join mutate arrange %>% filter select desc pull bind_rows
#' @importFrom gms chooseFromList
#' @importFrom rlang sym syms .data
#' @importFrom quitte as.quitte getModels getRegs getScenarios
#' @importFrom grDevices pdf dev.off
#' @importFrom mip plotstyle plotstyle.add showAreaAndBarPlots showLinePlots
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
                                renameModels = NULL, lineplotVariables = TRUE, areaplotVariables = TRUE,
                                interactive = FALSE, mainReg = "World", plotby = c("model", "scenario"), diffto = NULL,
                                yearsBarPlot = c(2030, 2050), postfix = format(Sys.time(), "_%Y-%m-%d_%H.%M.%S")) {

  if (!is.null(outputDirectory) && ! dir.exists(outputDirectory)) {
    dir.create(outputDirectory, recursive = TRUE)
  }
  stopifnot(is.null(plotby) || all(plotby %in% c("model", "scenario", "onefile")))
  if (isTRUE(interactive)) {
    interactive <- c("variable", "model", "scenario", "region", "period", "plotby", "diffto", "yearsBarPlot")
  }

  # load data
  data <- droplevels(quitte::as.quitte(mifFile, na.rm = TRUE))

  # prepare summation groups
  summationGroups <- data.frame(list(variable = "", child = ""))
  summationList <- list()
  if (isTRUE(summationsFile == "extractVariableGroups")) {
    summationList <- extractVariableGroups(levels(data$variable), keepOrigNames = FALSE, sorted = TRUE)
    summationList <- lapply(summationList, removePlus)
    summationGroups <- bind_rows(lapply(names(summationList),
                                        function(x) data.frame(parent = x, child = summationList[[x]], factor = 1)))
    data <- removePlus(data)
  } else if (! is.null(summationsFile)) {
    data <- removePlus(data)
    summationGroups <- getSummations(summationsFile)
    summationGroups$parent <- removePlus(summationGroups$parent)
    summationGroups$child <- removePlus(summationGroups$child)
    # only leave in areaplotVariables contained in summationFile and where at least one child is in the data
    for (i in intersect(unique(summationGroups$parent), unique(data$variable))) {
      childVariables <- intersect(summationGroups[which(summationGroups[, "parent"] == i), "child"],
                                  unique(data$variable))
      if (length(childVariables) > 0) {
        summationList[[i]] <- childVariables
      }
    }
  }

  # adjust data
  data <- data %>%
    left_join(summationGroups, by = c("variable" = "child")) %>%
    droplevels()
  if ("diffto" %in% interactive) {
    diffto <- chooseFromList(setdiff(levels(data$scenario), "historical"), multiple = FALSE,
                             userinfo = "Leave empty to get the normal absolute values.",
                             type = "scenario that serves as reference, so the difference to this scenario is plotted")
  }
  if (length(diffto) > 0) data <- diffToScen(data, diffto)
  hist <- filter(data,   .data$scenario %in% "historical") %>% droplevels()
  data <- filter(data, ! .data$scenario %in% "historical") %>% droplevels()

  # adjust model names
  newModelNames <- levels(data$model)
  for (n in names(renameModels)) {
    newModelNames <- gsub(n, renameModels[n], newModelNames, fixed = TRUE)
  }
  levels(data$model) <- newModelNames

  combineVariables <- function(vars) {
    if (length(vars) == 0 || isFALSE(vars)) return(NULL)
    mappingfiles <- vars[vars %in% names(mappingNames()) | file.exists(vars)]
    tmpVars <- setdiff(vars, mappingfiles)
    for (mapping in mappingfiles) {
      tmpVars <- c(vars, getMapping(mapping)$variable)
    }
    return(sort(removePlus(unique(tmpVars))))
  }

  lineplotVariables <- if (isTRUE(lineplotVariables)) unique(data$variable) else combineVariables(lineplotVariables)
  areaplotVariables <- if (isTRUE(areaplotVariables)) unique(data$variable) else combineVariables(areaplotVariables)

  # select the plot style if undefined
  if (is.null(plotby) || "plotby" %in% interactive) {
    plotby <- chooseFromList(c("onefile", "model", "scenario"), "pdfs to be generated",
                          userinfo = "all in one file, and/or one file per model, scenario")
  }
  if (any(c("model", "scenario", "region", "period") %in% interactive)) {
    data <- quitte::chooseFilter(data, types = intersect(c("model", "scenario", "region", "period"), interactive),
                                 keep = list(region = mainReg))
  }
  lineplotVariables <- sort(intersect(lineplotVariables, unique(data$variable)))
  availableForAreaplot <- removeNo(names(summationList)) %in% intersect(areaplotVariables, unique(data$variable))
  areaplotVariables <- summationList[sort(names(summationList)[availableForAreaplot])]

  if ("variable" %in% interactive) {
    chosen <- chooseFromList(unique(removeNo(names(areaplotVariables))),
                             userinfo = "Choose parent variables for area plots.",
                             type = "parent variables of area plots to be plotted")
    areaplotVariables <- areaplotVariables[removeNo(names(areaplotVariables)) %in% chosen]
    lineplotVariables <- chooseFromList(lineplotVariables,
                                        userinfo = "Choose variables for line plots.",
                                        type = "variables to be plotted with line plots")
  }

  if ("yearsBarPlot" %in% interactive && length(areaplotVariables) > 0) {
    default <- sort(head(intersect(c(2030, 2050, 2040, 2020, 2100, unique(data$period)), unique(data$period)), 2))
    chosen <- chooseFromList(unique(data$period), multiple = TRUE, type = "years used for bar plots",
                             userinfo = paste0("Choose years used for bar plots. Press Enter to select ",
                                        paste(default, collapse = ", "), "."))
    yearsBarPlot <- if (length(chosen) > 0) chosen else default
  }

  ps <- mip::plotstyle(as.character(runif(length(levels(data$model)))))
  output <- try(mip::plotstyle.add(levels(data$model), levels(data$model), ps, replace = TRUE))
  ps <- mip::plotstyle(as.character(runif(length(levels(data$scenario)))))
  output <- try(mip::plotstyle.add(levels(data$scenario), levels(data$scenario), ps, replace = TRUE))
  if (inherits(output, "try-error")) message("Error running mip::plotstyle.add, you may have inconsistent coloring.")

  countpdfs <- length(c(if ("scenario" %in% plotby && length(quitte::getModels(data)) > 1) quitte::getScenarios(data),
                        if ("model" %in% plotby && length(quitte::getScenarios(data)) > 1) quitte::getModels(data),
                        if ("onefile" %in% plotby) "onefile"))
  countpdfs <- max(1, countpdfs)

  message("### ", countpdfs, " documents will be generated, each with max. ",
          length(unique(c(removeNo(names(areaplotVariables)), lineplotVariables))), " variables. Enjoy waiting.\n")

  if ("scenario" %in% plotby && length(quitte::getModels(data)) > 1) {
    for (thisscenario in quitte::getScenarios(data)) {
      pdfFile <- file.path(outputDirectory, paste0("compare_models_", gsub(" ", "_", thisscenario), postfix, ".pdf"))
      message("\n## Writing ", pdfFile, " for scenario '", thisscenario, "'.\n")
      plotdata <- filter(data, .data$scenario == thisscenario) %>% droplevels()
      makepdf(pdfFile, plotdata, lineplotVariables, areaplotVariables, mainReg, diffto, hist, yearsBarPlot)
    }
  }
  if ("model" %in% plotby && length(quitte::getScenarios(data)) > 1) {
    for (thismodel in quitte::getModels(data)) {
      pdfFile <- file.path(outputDirectory, paste0("compare_scenarios_", gsub(" ", "_", thismodel), postfix, ".pdf"))
      message("\n## Writing ", pdfFile, " for model '", thismodel, "'.\n")
      plotdata <- filter(data, .data$model == thismodel) %>% droplevels()
      makepdf(pdfFile, plotdata, lineplotVariables, areaplotVariables, mainReg, diffto, hist, yearsBarPlot)
    }
  }
  if ("onefile" %in% plotby || (length(quitte::getModels(data)) == 1 && length(quitte::getScenarios(data)) == 1)) {
    pdfFile <- file.path(outputDirectory, paste0("compare_scenarios", postfix, ".pdf"))
    message("\n## Writing ", pdfFile, " with all data.\n")
    makepdf(pdfFile, droplevels(data), lineplotVariables, areaplotVariables, mainReg, diffto, hist, yearsBarPlot)
  }
  message("Done. See results in ", normalizePath(outputDirectory), ".")
}

makepdf <- function(pdfFilename, plotdata, lineplotVariables, areaplotVariables, mainReg,
                    diffto = NULL, hist = NULL, yearsBarPlot) {
  if (nrow(plotdata) == 0) {
    message("plotdata empty, skipping.")
    return()
  }
  if (! is.null(hist)) hist$identifier <- NA
  plotdata$identifier <- mip::identifierModelScen(plotdata)
  legendTitle <- paste(c(attr(plotdata$identifier, "deletedinfo"), "Model output")[[1]],
                       if (length(diffto) > 0) paste("difference to", diffto))
  if (! all(levels(plotdata$identifier) %in% names(mip::plotstyle()))) {
    ps <- mip::plotstyle(as.character(runif(length(levels(plotdata$identifier)))))
    try(mip::plotstyle.add(levels(plotdata$identifier), levels(plotdata$identifier), ps, replace = TRUE))
  }
  pdf(pdfFilename,
      width = 1.2 * max(12, length(quitte::getRegs(plotdata)),
                  length(quitte::getModels(plotdata)) * length(quitte::getScenarios(plotdata)) * 2),
      height = 1.2 * 9)
  plotvariables <- sort(unique(c(lineplotVariables, names(areaplotVariables))))
  plotvariables <- plotvariables[removeNo(plotvariables) %in% plotdata$variable]
  for (p in plotvariables) {
    message(which(p == plotvariables), "/", length(plotvariables), ": Add plot for ", p)
    # area plot
    if (p %in% names(areaplotVariables)) {
      childs <- intersect(areaplotVariables[[p]], unique(plotdata$variable))
      message("Childs: ", paste(gsub(p, "", childs, fixed = TRUE), collapse = ", "))
      e <- try(showAreaAndBarPlots(plotdata, if (length(childs) > 0) childs else removeNo(p), tot = removeNo(p),
                                   mainReg = mainReg, scales = "fixed",
                                   yearsBarPlot = intersect(yearsBarPlot, unique(plotdata$period))))
      if (inherits(e, "try-error")) {
        message("areaplot failed for ", p, ", do lineplot.")
        lineplotVariables <- c(lineplotVariables, removeNo(p)) # if area plot fails, do lineplot
      }
    }
    # line plot
    if (p %in% lineplotVariables) {
      showLinePlots(rbind(plotdata, hist), p, mainReg = mainReg, color.dim.name = legendTitle,
                    vlines = as.numeric(format(Sys.time(), "%Y")))
    }
  }
  dev.off()
  message("\n## Writing ", pdfFilename, " successful.\n")
}

diffToScen <- function(data, diffto) {
  stopifnot(
    `diffto has to have length 1` = length(diffto) == 1,
    `diffto has to be a scenario name present in the data` = diffto %in% levels(data$scenario)
  )
  dref <- data %>%
    filter(.data$scenario == diffto) %>%
    select(-"scenario") %>%
    rename("ref" = "value") %>%
    droplevels()
  # make sure also historical data is adjusted, if only one model is provided and diffto therefore unique
  histmodels <- unique(as.character(data$model[data$scenario == "historical"]))
  if (length(levels(dref$model) == 1) && length(histmodels) > 0) {
    dref <- bind_rows(list(dref, lapply(histmodels, function(x) mutate(dref, model = factor(x)))))
  }
  data <- data %>%
    left_join(dref, by = c("model", "region", "variable", "unit", "period")) %>%
    mutate("value" = !!sym("value") - !!sym("ref")) %>%
    filter(.data$scenario != diffto) %>%
    droplevels()
  return(data)
}

removeNo <- function(x) {
  return(gsub(" [1-9]$", "", x))
}
