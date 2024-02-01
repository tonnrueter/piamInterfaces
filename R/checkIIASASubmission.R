#' Check IIASA submission by comparing mif data to a template file (xlsx or yaml) provided by IIASA
#'
#' @md
#' @author Oliver Richters
#' @param mifdata quitte object or filename of mif file
#' @param iiasatemplate filename of xlsx or yaml file provided by IIASA
#' @param failOnUnitMismatch boolean whether to fail in case of unit mismatches
#'        recommended for submission, not used for generating templates
#' @param logFile filename of file for logging
#' @importFrom dplyr filter
#' @importFrom quitte read.quitte
#' @importFrom stringr str_split
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
  variable <- NULL # to avoid global binding note

  if (length(mifdata) == 1 && file.exists(mifdata)) {
    mifdata <- read.quitte(mifdata, factors = FALSE)
  }
  cat(paste0("\n### Load IIASA template file ", iiasatemplate, ".\n"))
  if (is.null(logFile)) {
    logFile <- stdout()
  } else {
    cat(paste0("# Find info on deleted variables, unit and data mismatches in ", logFile, ".\n"))
  }
  template <- loadIIASATemplate(iiasatemplate)

  varsNotInTemplate <- sort(unique(mifdata$variable[! mifdata$variable %in% template$variable]))

  cat(paste0("# ", length(varsNotInTemplate), " variables not in IIASA template are deleted.\n"))
  write(paste0("\n\n#--- ", length(varsNotInTemplate), " variables not in IIASA template ", iiasatemplate,
               " are deleted ---#"), file = logFile, append = TRUE)
  write(paste0("  - ", paste(varsNotInTemplate, collapse = "\n  - ")), file = logFile, append = TRUE)
  mifdata <- filter(mifdata, variable %in% template$variable)

  mifdata <- checkFixUnits(mifdata, template, logFile, failOnUnitMismatch = failOnUnitMismatch)

  # if data is suppled (not while generating mapping file), check whether scenarios have same number of variables
  if ("value" %in% names(mifdata)) {
    checkDataLength(mifdata, logFile)
  }

  return(mifdata)
}




checkDataLength <- function(mifdata, logFile =  NULL) {
  differingdatalength <- 0
  logtext <- "\n\n### Check whether all scenarios have same number of variables"
  for (vari in levels(mifdata$variable)) {
    countDataPoints <- seq_along(levels(mifdata$scenario))
    for (i in countDataPoints) {
      countDataPoints[i] <- sum(droplevels(filter(mifdata, !!sym("variable") == vari))$scenario
                             == levels(mifdata$scenario)[[i]])
    }
    if (length(unique(countDataPoints)) != 1) {
      logtext <- c(logtext,
        paste0("- For ", vari, ", data points per scenario differ: ",
        paste0(levels(mifdata$scenario), ": ", countDataPoints, ".", collapse = " "))
      )
      differingdatalength <- differingdatalength + 1
    }
  }
  if (differingdatalength == 0) {
    logtext <- c(logtext, "- Everything seems fine")
  } else {
    cat(paste0("# ", differingdatalength, " variables found whose data points differ between scenarios"))
  }
  if (length(logtext) > 0 && ! is.null(logFile)) {
    write(logtext, file = logFile, append = TRUE)
  }
}
