#' Check IIASA submission by comparing mif data to a template file (xlsx or yaml) provided by IIASA
#'
#' @md
#' @author Oliver Richters
#' @param mifdata quitte object or filename of mif file
#' @param iiasatemplate filename of xlsx or yaml file provided by IIASA
#' @param failOnUnitMismatch boolean whether to fail in case of unit mismatches
#'        recommended for submission
#' @param logFile filename of file for logging. Set to NULL for stdout, set to FALSE for none.
#' @importFrom dplyr filter
#' @importFrom quitte read.quitte
#' @importFrom stringr str_split
#' @importFrom utils capture.output
#' @return quitte object with adapted mif data
#' @examples
#' \dontrun{
#' # Simple use. Generates submission file in output folder:
#' checkIIASASubmission(
#'   mifdata = "file.mif",
#'   iiasatemplate = "template.xlsx",
#'   logFile = "logFile.txt"
#' )
#' }
#' @export
checkIIASASubmission <- function(mifdata, iiasatemplate, logFile = NULL, failOnUnitMismatch = TRUE) {
  mifdata <- droplevels(as.quitte(mifdata, na.rm = TRUE))
  cat(paste0("\n### Load IIASA template file ", iiasatemplate, ".\n"))
  if (! isFALSE(logFile)) {
    if (is.null(logFile)) {
      logFile <- stdout()
    } else {
      cat(paste0("# Find info on deleted variables, unit and data mismatches in ", logFile, ".\n"))
    }
  }
  template <- loadIIASATemplate(iiasatemplate)

  varsNotInTemplate <- mifdata %>%
    filter(! .data$variable %in% unique(template$variable)) %>%
    droplevels() %>%
    pull(.data$variable) %>%
    levels() %>%
    sort()

  cat(paste0("# ", length(varsNotInTemplate), " variables not in IIASA template are deleted.\n"))
  if (! isFALSE(logFile)) {
    write(paste0("\n\n#--- ", length(varsNotInTemplate), " variables not in IIASA template ", iiasatemplate,
                 " are deleted ---#"), file = logFile, append = TRUE)
    write(paste0("  - ", paste(varsNotInTemplate, collapse = "\n  - ")), file = logFile, append = TRUE)
  }
  mifdata <- filter(mifdata, .data$variable %in% template$variable)

  mifdata <- checkFixUnits(mifdata, template, logFile, failOnUnitMismatch = failOnUnitMismatch)

  # if data is supplied (not while generating mapping file), check whether scenarios have same number of variables
  if ("value" %in% names(mifdata)) {
    checkDataLength(mifdata, logFile)
  }

  return(mifdata)
}


checkDataLength <- function(mifdata, logFile =  NULL) {
  mifdata <- as.quitte(mifdata)
  logtext <- "\n\n### Check whether all scenarios have same number of variables"
  scens <- levels(mifdata$scenario)
  differing <- data.frame(matrix(1, nrow = 0, ncol = length(scens) + 1))
  colnames(differing) <- c("Variable", scens)
  for (vari in levels(mifdata$variable)) {
    countDataPoints <- seq_along(levels(mifdata$scenario))
    for (i in countDataPoints) {
      countDataPoints[i] <- sum(droplevels(filter(mifdata, !!sym("variable") == vari))$scenario
                                == levels(mifdata$scenario)[[i]])
    }
    if (length(unique(countDataPoints)) != 1) {
      differing[nrow(differing) + 1, ] <- c(vari, countDataPoints)
    }
  }
  if (nrow(differing) == 0) {
    logtext <- "\n\n### Check whether all scenarios have same number of variables is fine."
  } else {
    logtext <- c(paste("\n\n###", nrow(differing), "variables found whose data points differ between scenarios:"),
                 capture.output(print.data.frame(differing, print.gap = 2, quote = FALSE, right = FALSE)))
    cat(paste0("# ", nrow(differing), " variables found whose data points differ between scenarios."))
  }
  if (length(logtext) > 0 && ! is.null(logFile) && ! isFALSE(logFile)) {
    write(logtext, file = logFile, append = TRUE)
  }
}
