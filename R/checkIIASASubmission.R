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
  # use template units as names and map it to remind2 unit with identical meaning
  identicalUnits <- c("billion m2/yr" = "bn m2/yr",
                      "billion pkm/yr" = "bn pkm/yr",
                      "billion tkm/yr" = "bn tkm/yr",
                      "billion vkm/yr" = "bn vkm/yr",
                      "kt CF4/yr" = "kt CF4-equiv/yr",
                      "Million" = "million",
                      "Mt/yr" = "Mt/year",
                 NULL)

  if (length(mifdata) == 1 && file.exists(mifdata)) {
    mifdata <- read.quitte(mifdata, factors = FALSE)
  }
  cat(paste0("\n### Load IIASA template file ", iiasatemplate, ".\n"))
  if (is.null(logFile)) {
    logFile <- stdout()
  } else {
    cat(paste0("# Find info on deleted variables and unit mismatches in", logFile, ".\n"))
  }
  template <- loadIIASATemplate(iiasatemplate)

  varsNotInTemplate <- sort(unique(mifdata$variable[! mifdata$variable %in% template$variable]))
  if (length(varsNotInTemplate) > 0) {
    cat(paste0("# ", length(varsNotInTemplate), " variables not in IIASA template are deleted.\n"))
    write(paste0("\n\n#--- ", length(varsNotInTemplate), " variables not in IIASAtemplate ", iiasatemplate,
                 " are deleted ---#"), file = logFile, append = TRUE)
    write(paste0("  - ", paste(varsNotInTemplate, collapse = "\n  - ")), file = logFile, append = TRUE)
    mifdata <- filter(mifdata, variable %in% template$variable)
  }

  wrongUnits <- data.frame(variable = character(), templateunit = character(), mifunit = character())
  logtext <- NULL
  for (mifvar in unique(mifdata$variable)) {
    templateunit <- unique(template$unit[template$variable == mifvar])
    mifunit <- unique(mifdata$unit[mifdata$variable == mifvar])
    if (! all(mifunit %in% c(unlist(str_split(templateunit, " [Oo][Rr] ")), templateunit))) {
      if (length(identicalUnits) > 0 && templateunit %in% names(identicalUnits)
          && all(identicalUnits[[templateunit]] == mifunit)) {
        logtext <- c(logtext, paste0("  - for ", mifvar, ": ", mifunit, " -> ", templateunit, "."))
        mifdata$unit[mifdata$variable == mifvar] <- templateunit
      } else {
        wrongUnits[nrow(wrongUnits) + 1, ] <- c(mifvar, paste(templateunit, collapse = ","),
                                                       paste(mifunit, collapse = ","))
      }
    }
  }
  if (length(logtext) > 0) {
    write(paste0("\n\n#--- ", length(logtext), " units were corrected: ---#\n",
          paste0(logtext, collapse = "\n")), file = logFile, append = TRUE)
    cat(paste0("# ", length(logtext), " units were corrected.\n"))
  }
  if (nrow(wrongUnits) > 0) {
    cat(paste0("# ", nrow(wrongUnits), " unit mismatches between template and reporting.\n"))
    logtext <- paste0("\n\n#--- ", nrow(wrongUnits), " unit mismatches ---#")
    for (wno in seq_along(rownames(wrongUnits))) {
      w <- wrongUnits[wno, ]
      logtext <- c(logtext, paste0("  - '", w[[1]], "' uses '", w[[3]], "', but template requires '", w[[2]], "'."))
    }
    cat(paste0("If they are identical apart from spelling, ",
               "add them to vector 'identicalUnits' in piamInterfaces::checkIIASASubmission() as:\n"))
    unitsOnly <- unique(wrongUnits[c(2, 3)])
    for (wno in seq_along(rownames(unitsOnly))) {
      cat(paste0('                      "', unitsOnly[wno, 1], '" = "', unitsOnly[wno, 2], '",\n'))
    }
    write(logtext, file = logFile, append = TRUE)
    cat("\n")
    if (failOnUnitMismatch) stop("Unit mismatches!")
  }
  return(mifdata)
}
