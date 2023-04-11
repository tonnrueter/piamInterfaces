#' Check units in IIASA submission by comparing mifdata to a template
#'
#' @md
#' @author Oliver Richters
#' @param mifdata quitte object or filename of mif file
#' @param template object provided by loadIIASAtemplate()
#' @param failOnUnitMismatch boolean whether to fail in case of unit mismatches
#'        recommended for submission, not used for generating templates
#' @param logFile filename of file for logging
#' @importFrom dplyr filter
#' @importFrom stringr str_split
#' @return quitte object with adapted mif data
#' @export

checkFixUnits <- function(mifdata, template, logFile = NULL, failOnUnitMismatch = TRUE) {
  # use template units as names and map it to remind2 unit with identical meaning
  identicalUnits <- c("billion m2/yr" = "bn m2/yr",
                      "billion pkm/yr" = "bn pkm/yr",
                      "billion tkm/yr" = "bn tkm/yr",
                      "billion vkm/yr" = "bn vkm/yr",
                      "kt CF4/yr" = "kt CF4-equiv/yr",
                      "Million" = "million",
                      "Mt/yr" = "Mt/year",
                      # for backwards compatibility with AR6 and SHAPE templates (using old incorrect unit)
                      # should only affect template variable 'Energy Service|Residential and Commercial|Floor Space'
                      "billion m2/yr" = "bn m2",
                      "billion m2" = "bn m2/yr",
                      "bn m2/yr" = "bn m2",
                      "bn m2" = "bn m2/yr",
                 NULL)

  # try to identify and fix wrong units
  wrongUnits <- data.frame(variable = character(), templateunit = character(), mifunit = character())
  logtext <- NULL
  for (mifvar in unique(mifdata$variable)) {
    templateunit <- unique(template$unit[template$variable == mifvar])
    mifunit <- unique(mifdata$unit[mifdata$variable == mifvar])
    # find unit mismatches
    if (! all(mifunit %in% c(unlist(str_split(templateunit, " [Oo][Rr] ")), templateunit))) {
      if (length(identicalUnits) > 0 && templateunit %in% names(identicalUnits)
          && all(identicalUnits[[templateunit]] == mifunit)) {
        # fix wrong spelling of units as allowed in identicalUnits
        logtext <- c(logtext, paste0("  - for ", mifvar, ": ", mifunit, " -> ", templateunit, "."))
        mifdata$unit[mifdata$variable == mifvar] <- templateunit
      } else if (all(grepl("^Index \\([0-9]* = 1\\)$", mifunit))) {
        if ("value" %in% names(mifdata)) {
          logtext <- c(logtext, paste0("  - for ", mifvar, ": ", mifunit, " -> ", templateunit, ", data adapted."))
          referenceYear <- as.numeric(extractReferenceYear(templateunit))
          mifdata <- priceIndicesFix(mifdata, mifvar, referenceYear)
        }
      } else {
        # log units unable to fix
        wrongUnits[nrow(wrongUnits) + 1, ] <- c(mifvar, paste(templateunit, collapse = ","),
                                                       paste(mifunit, collapse = ","))
      }
    }
  }
  if (length(logtext) > 0) {
    cat(paste0("# ", length(logtext), " units were corrected.\n"))
    logtext <- paste0("\n\n#--- ", length(logtext), " units were corrected: ---#\n",
          paste0(logtext, collapse = "\n"))
  }

  if (nrow(wrongUnits) > 0) {
    logtext <- c(logtext, reportWrongUnits(wrongUnits))
    if (failOnUnitMismatch) stop("Unit mismatches!")
  }

  if (length(logtext) > 0 && ! is.null(logFile)) {
    write(logtext, file = logFile, append = TRUE)
  }

  return(mifdata)
}


# reporting function for units unable to fix, and what to do with them
reportWrongUnits <- function(wrongUnits) {
  cat(paste0("# ", nrow(wrongUnits), " unit mismatches between template and reporting.\n"))
  logtext <- paste0("\n\n#--- ", nrow(wrongUnits), " unit mismatches ---#")
  for (wno in seq_along(rownames(wrongUnits))) {
    w <- wrongUnits[wno, ]
    logtext <- c(logtext, paste0("  - '", w[[1]], "' uses '", w[[3]], "', but template requires '", w[[2]], "'."))
  }
  cat(paste0("If they are identical apart from spelling, ",
             "add them to vector 'identicalUnits' in piamInterfaces::checkFixUnits() as:\n"))
  unitsOnly <- unique(wrongUnits[c(2, 3)])
  for (wno in seq_along(rownames(unitsOnly))) {
    cat(paste0('                      "', unitsOnly[wno, 1], '" = "', unitsOnly[wno, 2], '",\n'))
  }
  cat("\n")
  return(logtext)
}
