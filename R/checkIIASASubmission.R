#' Check IIASA submission by comparing mif data to a template file (xlsx or yaml) provided by IIASA
#'
#' @md
#' @author Oliver Richters
#' @param mifdata quitte object or filename of mif file
#' @param iiasatemplate filename of xlsx or yaml file provided by IIASA
#' @param logfile filename of file for logging
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
#'   logfile = "logfile.txt"
#' )
#' }
#' @export
checkIIASASubmission <- function(mifdata, iiasatemplate, logfile) {
  variable <- NULL # to avoid global binding note
  meslog <- function(text, logfile) {
    message(text)
    write(text, file = logfile, append = TRUE)
  }

  # use template units as names and map it to remind2 unit with identical meaning
  identicalUnits <- c("billion m2/yr" = "bn m2/yr",
                      "billion pkm/yr" = "bn pkm/yr",
                      "billion tkm/yr" = "bn tkm/yr",
                      "billion vkm/yr" = "bn vkm/yr",
                      "kt CF4/yr" = "kt CF4-equiv/yr",
                      "Mt/yr" = "Mt/year"
                     )

  if (length(mifdata) == 1 && file.exists(mifdata)) {
    mifdata <- read.quitte(mifdata, factors = FALSE)
  }
  meslog(paste0("\n### Load IIASA template file ", iiasatemplate, "."), logfile)
  template <- loadIIASATemplate(iiasatemplate)

  varsNotInTemplate <- unique(mifdata$variable[! mifdata$variable %in% template$variable])

  message("# ", length(varsNotInTemplate), " variables not in IIASA template were deleted, see ", logfile, ".")
  write(paste0("\n\n#--- ", length(varsNotInTemplate), " variables not in IIASAtemplate ", iiasatemplate,
               " are deleted ---#"), file = logfile, append = TRUE)
  write(paste0("  - ", paste(varsNotInTemplate, collapse = "\n  - ")), file = logfile, append = TRUE)
  mifdata <- filter(mifdata, variable %in% template$variable)

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
          paste0(logtext, collapse = "\n")), file = logfile, append = TRUE)
    message("# ", length(logtext), " units were corrected, see ", logfile)
  }
  if (nrow(wrongUnits) > 0) {
    message("Unit mismatch between template and reporting, find details in ", logfile, ":")
    print(unique(wrongUnits[c(2, 3)]))
    logtext <- "Unit mismatches:\n\n"
    for (w in wrongUnits) {
      logtext <- c(logtext, paste0("- ", w[[1]], "' uses '", w[[2]], "', but template requires '", w[[3]], "."))
    }
    write(logtext, file = logfile, append = TRUE)
    message("If they are identical apart from spelling, ",
            "add them to vector 'identicalUnits' in scripts/output/export/xlsx_IIASA.R.")
    stop("Unit mismatches!")
  }
  return(mifdata)
}
