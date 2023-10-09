#' Checks for a run if the variables sum up as expected and logs spotted gaps
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param mifFile path to the mif file to apply summation checks to, or quitte object
#' @param dataDumpFile file where data.frame with the data analysis is saved. If NULL, result is returned
#' @param outputDirectory path to directory to place logFile and dataDumpFile
#' @param logFile file where human-readable summary is saved. If NULL, write to stdout
#' @param logAppend boolean whether to append or overwrite logFile
#' @param generatePlots boolean whether pdfs to compare data are generated
#' @param mainReg main region for the plot generation
#' @param summationsFile in inst/summations folder that describes the required summation groups
#'        if set to 'extractVariableGroups', tries to extract summations from variables with + notation
#' @param template mapping template to be loaded, used to print the remindVar corresponding to the data variables
#' @param remindVar REMIND/MAgPIE variable column name in template
#' @param plotprefix added before filename
#' @param absDiff threshold for absolute difference between parent variable and summation
#'                to be listed in human-readable summary
#' @param relDiff threshold (in percent) for relative difference between parent variable and summation
#'                to be listed in human-readable summary
#' @param roundDiff should the absolute and relative differences in human-readable summary
#'                  be rounded?
#' @importFrom dplyr group_by summarise ungroup left_join mutate arrange %>%
#'             filter select desc
#' @importFrom grDevices pdf dev.off
#' @importFrom magclass unitsplit
#' @importFrom mip showAreaAndBarPlots extractVariableGroups
#' @importFrom quitte as.quitte getModels getRegs getScenarios
#' @importFrom rlang sym syms
#' @importFrom stringr str_pad
#' @importFrom tibble tibble
#' @importFrom utils write.table
#'
#' @export
checkSummations <- function(mifFile, outputDirectory = ".", template = NULL, summationsFile = NULL,
                            logFile = NULL, logAppend = FALSE, generatePlots = FALSE, mainReg = "World",
                            dataDumpFile = "checkSummations.csv", remindVar = "piam_variable",
                            plotprefix = NULL, absDiff = 0.001, relDiff = 1, roundDiff = TRUE) {
  if (!is.null(outputDirectory) && !dir.exists(outputDirectory) && ! is.null(c(logFile, dataDumpFile))) {
    dir.create(outputDirectory, recursive = TRUE)
  }

  data <- quitte::as.quitte(mifFile, na.rm = TRUE)

  if (isTRUE(summationsFile == "extractVariableGroups")) {
    checkVariables <- extractVariableGroups(levels(data$variable), keepOrigNames = TRUE)
    names(checkVariables) <- make.unique(names(checkVariables), sep = " ")
  } else {
    summationGroups <- getSummations(summationsFile)
    if (summationsFile %in% names(summationsNames())) {
      summationsFile <- gsub(".*piamInterfaces", "piamInterfaces", summationsNames(summationsFile))
    }

    checkVariables <- list()

    # generate list of summation rules from summations file
    for (i in unique(summationGroups$parent)) {
      checkVariables[[i]] <- summationGroups[summationGroups[, "parent"] == i, "child"]
    }
  }

  parentVariables <- gsub(" [1-9]$", "", names(checkVariables))

  data <- data %>%
    filter(!!sym("variable") %in% unique(c(parentVariables, unlist(checkVariables, use.names = FALSE))))
  message("The filtered data contains ", length(unique(data$variable)), " variables.")

  if (nrow(data) == 0) {
    return(NULL)
  }

  # start with an empty tibble, such that return values always have the same
  # structure
  tmp <- tibble(model = factor(), scenario = factor(),
                          region = factor(), period = integer(),
                          variable = character(), unit = factor(),
                          value = numeric(), checkSum = numeric(),
                          diff = numeric(), reldiff = numeric())

  # iterate over summation rules
  for (i in seq_along(checkVariables)) {
    parentVar <- parentVariables[i]

    # skip summation rules that are not part of the data
    if (!(parentVar %in% unique(data$variable)) || !any(checkVariables[[i]] %in% unique(data$variable))) next

    # create comparison for summation rule
    parent <- filter(data, !!sym("variable") == parentVar) %>%
      mutate(variable = names(checkVariables[i]))
    children <- filter(data, !!sym("variable") %in% checkVariables[[i]]) %>%
      rename("child" = !!sym("variable"), "childVal" = !!sym("value"))
    comp <- left_join(parent, children, by = c("model", "scenario", "region", "period")) %>%
      # adapt unit of parent variable
      mutate(!!sym("unit") := !!sym("unit.x")) %>%
      select(-c("unit.x", "unit.y"))

    if (isTRUE(summationsFile == "extractVariableGroups")) {
      comp$factor <- 1
    } else {
      summation <- filter(summationGroups, parent == names(checkVariables[i]))
      comp <- left_join(comp, summation, by = c("child", "variable" = "parent"))
    }

    # calculate differences for comparison
    comp <- comp %>%
      group_by(!!!syms(c("model", "scenario", "region", "period", "variable", "unit", "value"))) %>%
      summarise(checkSum = sum(!!sym("childVal") * !!sym("factor")), .groups = "drop") %>%
      ungroup() %>%
      mutate(
        diff = !!sym("checkSum") - !!sym("value"),
        reldiff = 100 * (!!sym("checkSum") - !!sym("value")) / !!sym("value")
      )

    tmp <- rbind(tmp, comp)
  }

  # write data to dataDumpFile
  if (!is.null(outputDirectory) && length(dataDumpFile) > 0) {
    dataDumpFile <- file.path(outputDirectory, dataDumpFile)
    write.table(
      arrange(tmp, desc(abs(!!sym("reldiff")))), sep = ";",
      file = dataDumpFile, quote = FALSE, row.names = FALSE)
  }

  # generate human-readable summary of larger differences
  .checkSummationsSummary(
    mifFile, data, tmp, template, summationsFile, checkVariables,
    generatePlots, mainReg, outputDirectory, logFile, logAppend, dataDumpFile, remindVar,
    plotprefix, absDiff, relDiff, roundDiff
  )

  return(invisible(tmp))
}

.checkSummationsSummary <- function(mifFile, data, tmp, template, summationsFile, # nolint: cyclocomp_linter.
                             checkVariables, generatePlots, mainReg, outputDirectory, logFile, logAppend,
                             dataDumpFile, remindVar, plotprefix, absDiff, relDiff, roundDiff) {

  text <- paste0("\n### Analyzing ", if (is.null(ncol(mifFile))) mifFile else "provided data",
                 ".\n# Use ", summationsFile, " to check if summation groups add up.")
  summarytext <- NULL
  if (! is.null(template)) {
    if (length(template) == 1 && is.character(template) && template %in% names(templateNames())) {
      templateData <- getTemplate(template)
      templateName <- gsub(".*piamInterfaces", "piamInterfaces", templateNames(template))
      text <- c(text, paste0("# Derive mapping from ", templateName))
    } else {
      templateName <- "supplied template"
      templateData <- data.frame(template)
    }
  }
  for (thismodel in quitte::getModels(data)) {
    text <- c(text, paste0("# Analyzing results of model ", thismodel))
    fileLarge <- filter(tmp, abs(!!sym("reldiff")) >= relDiff,
                        abs(!!sym("diff")) >= absDiff, !!sym("model") == thismodel)
    problematic <- sort(unique(c(fileLarge$variable)))
    if (length(problematic) > 0) {
      if (generatePlots) {
        pdfFilename <- file.path(outputDirectory,
                                 paste0(plotprefix, "checkSummations_", gsub(" ", "_", thismodel), ".pdf"))
        pdf(pdfFilename,
            width = max(12, length(quitte::getRegs(fileLarge)), length(quitte::getScenarios(fileLarge)) * 2))
        plotdata <- filter(data, !!sym("model") == thismodel)
        message(length(problematic), " plots will be generated for ", thismodel, ", this will take some time.")
      }
      width <- 70
      text <- c(text, paste0("\n", str_pad(paste0("variable groups found in ",
                             basename(summationsFile)), width + 8, "right"),
        if (! is.null(template)) paste0("corresponding REMIND/MAgPIE variables extracted from ", basename(templateName))
         ))
      for (p in problematic) {
        signofdiff <- paste0("<"[max(fileLarge$diff[fileLarge$variable == p]) > 0],
                             ">"[min(fileLarge$diff[fileLarge$variable == p]) < 0])

        childs <- checkVariables[[p]]

        remindchilds <- if (is.null(template)) NULL else
                        unitsplit(templateData[, remindVar][unitsplit(templateData$Variable)$variable == p])$variable
        text <- c(text, paste0("\n", str_pad(paste(p, signofdiff), width + 5, "right"), "   ",
                  paste0(paste0(remindchilds, collapse = " + "), " ", signofdiff)[! is.null(remindchilds)]
                  ))
        for (ch in childs) {
          remindch <- if (is.null(template)) NULL else
                      unitsplit(templateData[, remindVar][unitsplit(templateData$Variable)$variable == ch])$variable
          text <- c(text, paste0("   + ", str_pad(ch, width, "right"),
                    if (! is.null(remindch)) paste0("      + ", paste0(remindch, collapse = " + "))))
        }

        reldDiffMax <- max(-fileLarge$reldiff[fileLarge$variable == p])
        relDiffMin <- min(-fileLarge$reldiff[fileLarge$variable == p])
        absDiffMax <- max(abs(fileLarge$diff[fileLarge$variable == p]))

        if (roundDiff) {
          reldDiffMax <- niceround(reldDiffMax)
          relDiffMin <- niceround(relDiffMin)
          absDiffMax <- niceround(absDiffMax)
        }

        text <- c(text, paste0("Relative difference between ",
                          relDiffMin, "% and ",
                          reldDiffMax, "%, ",
                          "absolute difference up to ",
                          absDiffMax, " ",
                          paste0(unique(fileLarge$unit[fileLarge$variable == p]), collapse = ", "), ".")
        )
        if (generatePlots) {
          message("Add plot for ", p)
          mip::showAreaAndBarPlots(plotdata, intersect(childs, unique(plotdata$variable)),
                                   tot = gsub(" [1-9]$", "", p),
                                   mainReg = mainReg, yearsBarPlot = c(2030, 2050), scales = "fixed")
        }
      }
      # print to log or stdout
      summarytext <- c(summarytext, paste0("\n# Summary of summation group checks for model ", thismodel, ":"),
        paste0("# ", length(problematic), " equations are not satisfied but should according to ",
              basename(summationsFile), "."),
        paste0("# All deviations can be found in the returned object",
               paste0(" and in ", dataDumpFile)[! is.null(dataDumpFile)], "."),
        paste0("# To get more detailed information on '", problematic[1], "', run piamInterfaces::variableInfo('",
               problematic[1], "').")
        )
      if (generatePlots) {
        dev.off()
        summarytext <- c(summarytext, paste0("\n# Find plot comparison of all errors in ", pdfFilename))
      }
    } else {
      summarytext <- c(summarytext, paste0("\n# All summation checks were fine for model ", thismodel, "."))
    }
  }
  summarytext <- c(summarytext, "\n# As generatePlots=FALSE, no plot comparison was produced."[! generatePlots])
  if (is.null(logFile)) {
    message(paste(c(text, summarytext, ""), collapse = "\n"))
  } else {
    if (! is.null(outputDirectory)) logFile <- file.path(outputDirectory, logFile)
    message(paste(c(text[1], summarytext,
            paste0("# Find log with human-readable information appended to ", logFile)), "", collapse = "\n"))
    write(c(text, summarytext, ""), file = logFile, append = logAppend)
  }
}
