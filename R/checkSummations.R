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
#' @param summationsFile in inst/summations folder that describes the required summation groups
#' @param template mapping template to be loaded
#' @param remindVar REMIND/MAgPIE variable column name in template
#' @importFrom dplyr group_by summarise ungroup left_join mutate arrange %>% filter select desc
#' @importFrom magclass unitsplit
#' @importFrom mip showAreaAndBarPlots
#' @importFrom rlang sym syms
#' @importFrom utils write.table
#' @importFrom stringr str_pad
#' @importFrom quitte as.quitte getModels getRegs getScenarios
#' @importFrom grDevices pdf dev.off
#'
#' @export
checkSummations <- function(mifFile, outputDirectory = ".", template = "AR6", summationsFile = "AR6",
                            logFile = NULL, logAppend = FALSE, generatePlots = FALSE,
                            dataDumpFile = "checkSummations.csv", remindVar = "piam_variable") {
  if (!is.null(outputDirectory) && !dir.exists(outputDirectory) && ! is.null(c(logFile, dataDumpFile))) {
    dir.create(outputDirectory, recursive = TRUE)
  }

  .calculateCheckSum <- function(name, x) {
    tmp <- x %>%
      group_by(!!!syms(c("model", "scenario", "region", "period"))) %>%
      summarise(checkSum = sum(!!sym("value") * !!sym("factor")), .groups = "drop") %>%
      ungroup()
    tmp$variable <- name
    return(tmp)
  }

  summationGroups <- getSummations(summationsFile)
  if (summationsFile %in% names(summationsNames())) {
    summationsFile <- gsub(".*piamInterfaces", "piamInterfaces", summationsNames(summationsFile))
  }

  data <- quitte::as.quitte(mifFile, na.rm = TRUE) %>%
    filter(!!sym("variable") %in% unique(c(summationGroups$child, summationGroups$parent))) %>%
    left_join(summationGroups, by = c("variable" = "child"))
  message("The filtered data contains ", length(unique(data$variable)), " variables.")
  checkVariables <- list()

  for (i in unique(summationGroups$parent)) {
    checkVariables[[i]] <- summationGroups[which(summationGroups[, "parent"] == i), "child"]
  }

  names(checkVariables) <- gsub(" [1-9]$", "", names(checkVariables))

  tmp <- NULL
  for (i in names(checkVariables)) {
    tmp <- rbind(tmp, .calculateCheckSum(
      i,
      filter(data, !!sym("parent") == i, !!sym("variable") %in% checkVariables[[i]])
    ))
  }

  tmp <- left_join(tmp, data, c("scenario", "region", "variable", "period", "model")) %>%
    mutate(
      diff = !!sym("checkSum") - !!sym("value"),
      reldiff = 100 * (!!sym("checkSum") - !!sym("value")) / !!sym("value")
    ) %>%
    select(-c("factor", "parent"))

  # write data to dataDumpFile
  if (length(dataDumpFile) > 0) {
    dataDumpFile <- file.path(outputDirectory, dataDumpFile)
    write.table(
      arrange(tmp, desc(abs(!!sym("reldiff")))), sep = ";",
      file = dataDumpFile, quote = FALSE, row.names = FALSE)
  }
  # generate human-readable summary of larger differences
  .checkSummationsSummary(mifFile, data, tmp, template, summationsFile, summationGroups,
                   generatePlots, outputDirectory, logFile, logAppend, dataDumpFile, remindVar)

  return(invisible(tmp))
}

.checkSummationsSummary <- function(mifFile, data, tmp, template, summationsFile, summationGroups,
                             generatePlots, outputDirectory, logFile, logAppend, dataDumpFile, remindVar) {
  text <- paste0("\n### Analyzing ", if (is.null(ncol(mifFile))) mifFile else "provided data",
                 ".\n# Use ", summationsFile, " to check if summation groups add up.")
  summarytext <- NULL
  if (! is.null(template)) {
    templateData <- getTemplate(template)
    templateName <- template
    if (template %in% names(templateNames())) {
      templateName <- gsub(".*piamInterfaces", "piamInterfaces", templateNames(template))
    }
    text <- c(text, paste0("# Derive mapping from ", templateName))
  }
  for (thismodel in quitte::getModels(data)) {
    text <- c(text, paste0("# Analyzing results of model ", thismodel))
    fileLarge <- filter(tmp, abs(!!sym("reldiff")) >= 1, abs(!!sym("diff")) >= 0.001, !!sym("model") == thismodel)
    problematic <- unique(c(fileLarge$variable))
    if (length(problematic) > 0) {
      if (generatePlots) {
        pdfFilename <- file.path(outputDirectory, paste0("checkSummations_", gsub(" ", "_", thismodel), ".pdf"))
        pdf(pdfFilename,
            width = max(12, length(quitte::getRegs(fileLarge)), length(quitte::getScenarios(fileLarge)) * 2))
        plotdata <- filter(data, !!sym("model") == thismodel)
        message(length(problematic), " plots will be generated for ", thismodel, ", this will take some time.")
      }
      width <- 70
      text <- c(text, paste0("\n", str_pad(paste0("variable groups found in ",
                             basename(summationsFile)), width + 8, "right"),
           if (! is.null(template)) paste0("corresponding REMIND/MAgPIE variables extracted from ", basename(template))
         ))
      for (p in problematic) {
        signofdiff <- paste0("<"[max(fileLarge$diff[fileLarge$variable == p]) > 0],
                             ">"[min(fileLarge$diff[fileLarge$variable == p]) < 0])
        childs <- summationGroups$child[summationGroups$parent == p]
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
        text <- c(text, paste0("Relative difference between ",
                          round(min(-fileLarge$reldiff[fileLarge$variable == p]), digits = 1), "% and ",
                          round(max(-fileLarge$reldiff[fileLarge$variable == p]), digits = 1), "%, ",
                          "absolute difference up to ",
                          round(max(abs(fileLarge$diff[fileLarge$variable == p])), digits = 2), " ",
                          paste0(unique(fileLarge$unit[fileLarge$variable == p]), collapse = ", "), ".")
        )
        if (generatePlots) {
          message("Add plot for ", p)
          mip::showAreaAndBarPlots(plotdata, intersect(childs, unique(plotdata$variable)), tot = p,
                                   mainReg = "World", yearsBarPlot = c(2030, 2050), scales = "fixed")
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
