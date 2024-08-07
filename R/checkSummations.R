#' Checks for a run if the variables sum up as expected and logs spotted gaps
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param mifFile path to the mif file to apply summation checks to, or quitte object
#' @param dataDumpFile file where data.frame with the data analysis is saved. Requires outputDirectory.
#'        If NULL, result is returned.
#' @param outputDirectory path to directory to place logFile and dataDumpFile.
#' @param logFile file where human-readable summary is saved. If NULL, write to stdout. If FALSE, don't log.
#' @param logAppend boolean whether to append or overwrite logFile
#' @param generatePlots boolean whether pdfs to compare data are generated. Requires outputDirectory.
#' @param mainReg main region for the plot generation
#' @param summationsFile in inst/summations folder that describes the required summation groups
#'        if set to 'extractVariableGroups', tries to extract summations from variables with + notation
#' @param template mapping to be loaded, used to print the remindVar corresponding to the data variables
#' @param remindVar REMIND/MAgPIE variable column name in 'template'
#' @param plotprefix added before filename
#' @param absDiff threshold for absolute difference between parent variable and summation
#' @param relDiff threshold (in percent) for relative difference between parent variable and summation
#' @param roundDiff should the absolute and relative differences in human-readable summary and dataDumpFile
#'                  be rounded? The returned object always contains unrounded values.
#' @param csvSeparator separator for dataDumpFile, defaults to semicolon
#' @importFrom dplyr group_by summarise ungroup left_join mutate arrange %>%
#'             filter select desc reframe last_col
#' @importFrom gms chooseFromList
#' @importFrom grDevices pdf dev.off
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
                            plotprefix = NULL, absDiff = 0.001, relDiff = 1, roundDiff = TRUE, csvSeparator = ";") {
  if (is.null(outputDirectory)) {
    dataDumpFile <- NULL
    generatePlots <- FALSE
  } else {
    dir.create(outputDirectory, recursive = TRUE, showWarnings = FALSE)
    logFile <- setLogFile(outputDirectory, logFile)
  }
  mapping <- template # did not want to change the function parameter name

  data <- quitte::as.quitte(mifFile, na.rm = TRUE)

  if (is.null(summationsFile)) {
    summationsOptions <- c("extractVariableGroups", names(summationsNames()))
    summationsFile <- chooseFromList(summationsOptions, multiple = FALSE, type = "summation file")
  }
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

  if (nrow(data) == 0) {
    warning("No variable found that matches summationsFile=", paste(summationsFile, collapse = ", "))
    return(NULL)
  }

  # start with an empty tibble, such that return values always have the same
  # structure
  comparison <- tibble(model = factor(), scenario = factor(),
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
      mutate("unit" = .data$unit.x) %>%
      select(-c("unit.x", "unit.y"))

    # add NA entries for children in summation rule not found in data
    comp <- merge(comp,
      comp %>%
      group_by(!!!syms(c("model", "scenario", "region", "period", "variable", "unit", "value"))) %>%
      reframe(child = checkVariables[[i]]),
      by = c("model", "scenario", "region", "period", "variable", "unit", "value", "child"), all.y = TRUE)

    if (isTRUE(summationsFile == "extractVariableGroups")) {
      comp$factor <- 1
    } else {
      summation <- filter(summationGroups, parent == names(checkVariables[i]))
      comp <- left_join(comp, summation, by = c("child", "variable" = "parent"))
    }

    # calculate differences for comparison
    comp <- comp %>%
      group_by(!!!syms(c("model", "scenario", "region", "period", "variable", "unit", "value"))) %>%
      summarise(checkSum = sum(!!sym("childVal") * !!sym("factor"), na.rm = TRUE),
        details = paste(
          ifelse(
            !!sym("factor") == 1,
            paste0(!!sym("child"), " (", !!sym("childVal"), ")"),
            paste0(!!sym("factor"), " * ", !!sym("child"), " (", !!sym("childVal"), ")")
          ),
          collapse = " + "),
        .groups = "drop") %>%
      ungroup() %>%
      mutate(diff = !!sym("checkSum") - !!sym("value")) %>%
      mutate(
        reldiff = ifelse(abs(!!sym("checkSum")) + abs(!!sym("value")) == 0, 0, 100 * !!sym("diff") / !!sym("value")),
        details = gsub("\\+ \\-", "-", !!sym("details"))
      ) %>%
      relocate(!!sym("details"), .after = last_col())

    comparison <- rbind(comparison, comp)
  }

  comparison <- comparison %>%
    filter(abs(.data$reldiff) >= relDiff, abs(.data$diff) >= absDiff) %>%
    arrange(desc(abs(.data$reldiff)))

  # write data to dataDumpFile
  if (!is.null(outputDirectory) && length(dataDumpFile) > 0) {
    dataDumpFile <- file.path(outputDirectory, dataDumpFile)
    filedata <- comparison
    if (isTRUE(roundDiff)) {
      filedata <- comparison %>% mutate(reldiff = niceround(.data$reldiff, digits = 2))
    }
    write.table(
      filedata,
      file = dataDumpFile,
      sep = csvSeparator,
      quote = FALSE,
      row.names = FALSE
    )
  }

  # generate human-readable summary of larger differences
  .checkSummationsSummary(
    mifFile, data, comparison, mapping, summationsFile, summationGroups, checkVariables,
    generatePlots, mainReg, outputDirectory, logFile, logAppend, dataDumpFile, remindVar,
    plotprefix, roundDiff
  )

  return(invisible(comparison))
}

.checkSummationsSummary <- function(mifFile, data, comparison, mapping, summationsFile, # nolint: cyclocomp_linter.
                                    summationGroups, checkVariables, generatePlots,
                                    mainReg, outputDirectory, logFile, logAppend,
                                    dataDumpFile, remindVar, plotprefix, roundDiff) {

  text <- paste0("\n### Analyzing ", if (is.null(ncol(mifFile))) mifFile else "provided data",
                 ".\n# Use ", summationsFile, " to check if summation groups add up.")
  summarytext <- NULL
  if (! is.null(mapping)) {
    if (length(mapping) == 1 && is.character(mapping) && mapping %in% names(mappingNames())) {
      mappingData <- getMapping(mapping)
      mappingName <- gsub(".*piamInterfaces", "piamInterfaces", mappingNames(mapping))
      text <- c(text, paste0("# Derive mapping from ", mappingName))
    } else {
      mappingName <- "supplied mapping"
      mappingData <- data.frame(mapping)
    }
  }

  for (thismodel in quitte::getModels(data)) {
    text <- c(text, paste0("# Analyzing results of model ", thismodel))
    fileLarge <- droplevels(filter(comparison, .data$model == thismodel, abs(.data$diff) > 0))
    text <- c(text, paste("# Run summation check on a total of", length(levels(data$variable)), "variables."))
    problematic <- sort(unique(c(fileLarge$variable)))
    if (length(problematic) == 0) {
      if (length(levels(data$variable)) > 0) {
        summarytext <- c(summarytext, paste0("\n# All summation checks were fine for model ", thismodel, "."))
      }
    } else {
      if (generatePlots) {
        pdfFilename <- file.path(outputDirectory,
                                 paste0(plotprefix, "checkSummations_", gsub(" ", "_", thismodel), ".pdf"))
        pdf(pdfFilename,
            width = max(12, length(quitte::getRegs(fileLarge)), length(quitte::getScenarios(fileLarge)) * 2))
        plotdata <- filter(data, .data$model == thismodel)
        message(length(problematic), " plots will be generated for ", thismodel, ", this will take some time.")
      }
      width <- 70
      text <- c(text, paste0("\n", str_pad(paste0("variable groups found in ",
                             basename(summationsFile)), width + 8, "right"),
        if (! is.null(mapping)) paste0("corresponding REMIND/MAgPIE variables extracted from ", basename(mappingName))
         ))
      for (p in problematic) {
        pn <- gsub(" [1-9]$", "", p)
        signofdiff <- paste0("<"[max(fileLarge$diff[fileLarge$variable == p]) > 0],
                             ">"[min(fileLarge$diff[fileLarge$variable == p]) < 0])

        childs <- checkVariables[[p]]

        remindchilds <- if (is.null(mapping)) NULL else
                        mappingData[, remindVar][mappingData$variable %in% pn]
        text <- c(text, paste0("\n", str_pad(paste(p, signofdiff), width + 5, "right"), "   ",
                  paste0(paste0(remindchilds, collapse = " + "), " ", signofdiff)[! is.null(remindchilds)]
                  ))
        for (ch in childs) {
          remindch <- if (is.null(mapping)) NULL else
                      mappingData[, remindVar][mappingData$variable %in% ch]
          text <- c(text, paste0("   + ", str_pad(ch, width, "right"),
                    if (! is.null(remindch)) paste0("      + ", paste0(remindch, collapse = " + "))))
        }

        relDiffMin <- min(fileLarge$reldiff[fileLarge$variable == p])
        relDiffMax  <- max(fileLarge$reldiff[fileLarge$variable == p])
        absDiffMax  <- max(abs(fileLarge$diff[fileLarge$variable == p]))

        if (roundDiff) {
          relDiffMin <- niceround(relDiffMin)
          relDiffMax <- niceround(relDiffMax)
          absDiffMax <- niceround(absDiffMax)
        }

        text <- c(text, paste0("The child sum differs by ",
                          relDiffMin, "% to ",
                          relDiffMax, "% from the parent, ",
                          "absolute difference up to ",
                          absDiffMax, " ",
                          paste0(unique(fileLarge$unit[fileLarge$variable == p]), collapse = ", "), ".")
        )
        childMissing <- childs[!childs %in% data$variable]
        if (length(childMissing) > 0) {
          text <- c(text, paste0("Variables not found in the data: ", toString(childMissing)))
        }
        if (generatePlots) {
          message("Add plot for ", p)
          s <- summationGroups %>%
            filter(.data$parent == p) %>%
            select(c("child", "factor"))

          df <- data %>%
            left_join(s, by = c("variable" = "child")) %>%
            mutate("value" = ifelse(is.na(.data$factor), .data$value, .data$value * .data$factor),
                   "unit" = filter(plotdata, .data$variable == pn)$unit[[1]]) %>%
            select(-"factor")

          mip::showAreaAndBarPlots(df, intersect(childs, unique(plotdata$variable)),
            tot = pn,
            mainReg = mainReg, yearsBarPlot = c(2030, 2050), scales = "fixed"
          )
        }
      }
      # print to log or stdout
      summarytext <- c(summarytext, paste0("\n# Summary of summation group checks for model ", thismodel, ":"),
        paste0("# ", length(problematic), " equations are not satisfied but should according to ",
              basename(summationsFile), "."),
        paste0("# All deviations can be found in the returned object",
               paste0(" and in ", dataDumpFile)[! is.null(dataDumpFile)], "."),
        paste0("# To get more detailed information on '", p, "', run piamInterfaces::variableInfo('",
               pn, "').")
        )
      if (generatePlots) {
        dev.off()
        summarytext <- c(summarytext, paste0("\n# Find plot comparison of all errors in ", pdfFilename))
      }
    }
  }
  summarytext <- c(summarytext, "\n# As generatePlots=FALSE, no plot comparison was produced."[! generatePlots])
  if (is.null(logFile)) {
    message(paste(c(text, summarytext, ""), collapse = "\n"))
  } else if (! isFALSE(logFile)) {
    message(paste(c(text[1], summarytext,
            paste0("# Find log with human-readable information appended to ", logFile)), "", collapse = "\n"))
    write(c(text, summarytext, ""), file = logFile, append = logAppend)
  }
}
