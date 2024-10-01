#' Check units in IIASA submission by comparing mifdata to a project template
#'
#' @md
#' @author Oliver Richters
#' @param mifdata quitte object or filename of mif file
#' @param template object provided by loadIIASAtemplate() or getMapping()
#'        interprets it as a mapping if 'piam_variable' and 'piam_unit' columns exist
#' @param failOnUnitMismatch boolean whether to fail in case of unit mismatches
#'        recommended for submission, not used for generating mappings
#' @param logFile filename of file for logging
#' @importFrom dplyr filter mutate
#' @importFrom GDPuc convertSingle
#' @importFrom piamutils deletePlus niceround
#' @importFrom rlang .data
#' @importFrom stringr str_split
#' @return quitte object with adapted mif data
#' @export

checkFixUnits <- function(mifdata, template, logFile = NULL, failOnUnitMismatch = TRUE) {  # nolint: cyclocomp_linter
  haspiam <- all(c("piam_variable", "piam_unit") %in% colnames(template))
  unitcol <- if (haspiam) "piam_unit" else "unit"
  varcol <- if (haspiam) "piam_variable" else "variable"

  template[[varcol]] <- deletePlus(template[[varcol]])
  mifdata <- deletePlus(droplevels(as.quitte(mifdata)))

  # try to identify and fix wrong units
  wrongUnits <- data.frame(variable = character(), templateunit = character(), mifunit = character())
  logtext <- NULL
  for (mifvar in intersect(levels(mifdata$variable), unique(template[[varcol]]))) {
    templateunit <- unique(template[[unitcol]][template[[varcol]] %in% mifvar])
    if (length(templateunit) != 1) stop(mifvar, " not mapped to unique unit: ", paste(templateunit, collapse = ", "))
    mifunit <- levels(droplevels(filter(mifdata, .data$variable %in% mifvar))$unit)
    # find and potentially fix unit mismatches
    if (! all(mifunit %in% c(unlist(str_split(templateunit, " [Oo][Rr] ")), templateunit))) {
      if (length(templateunit) > 1 || length(mifunit) > 1) {
        warning("Non-unique units for ", mifvar, ": templateunit: ", paste(templateunit, collapse = ", "),
                ". mifunit: ", paste(mifunit, collapse = ", "))
      }
      if (all(areUnitsIdentical(c(mifunit, templateunit)))) {
        # fix wrong spelling of units as allowed in identicalUnits
        logtext <- c(logtext, paste0("  - for ", mifvar, ": ", mifunit, " -> ", templateunit, "."))
        mifdata <- mifdata %>%
          mutate(unit = factor(ifelse(.data$variable == mifvar, templateunit, as.character(.data$unit))))
      } else if (all(grepl("^Index \\([0-9]* = 1\\)$", mifunit))) {
        # adjust different reference year for Index values
        if ("value" %in% names(mifdata)) {
          referenceYear <- as.numeric(extractReferenceYear(templateunit))
          mifdata <- priceIndicesFix(mifdata, mifvar, referenceYear)
          logtext <- c(logtext, paste0("  - for ", mifvar, ": ", mifunit, " -> ", templateunit, ", data adapted."))
        }
      } else if (all(gsub("US\\$(20)?05|USDMER05", "US$2017", mifunit) == templateunit)) {
        # convert US$2005 to US$2017 for backwards compatibility with old REMIND setting
        if ("value" %in% names(mifdata)) {
          convfact <- convertSingle(x = 1, iso3c = "USA",
                                    unit_in = "constant 2005 Int$PPP", unit_out = "constant 2017 Int$PPP")
          if (grepl("/.*US\\$(20)?05|/.*USDMER05", mifunit)) convfact <- 1 / convfact
          mifdata <- mutate(mifdata,
                       unit = factor(ifelse(.data$variable %in% mifvar, templateunit, as.character(.data$unit))),
                       value = ifelse(.data$variable %in% mifvar, round(.data$value * convfact, 7), .data$value)) %>%
                     droplevels()
          logtext <- c(logtext, paste0("  - for ", mifvar, ": ", mifunit, " -> ", templateunit,
                                       ", data multiplied by ", niceround(convfact, 4), "."))
        }
      } else {
        # log units unable to fix
        wrongUnits[nrow(wrongUnits) + 1, ] <- c(mifvar, paste(templateunit, collapse = ","),
                                                       paste(mifunit, collapse = ","))
      }
    }
  }

  reportCheckFixUnits(logtext, wrongUnits, logFile, failOnUnitMismatch)

  return(mifdata)
}

# collect reporting and fail if needed
reportCheckFixUnits <- function(logtext, wrongUnits, logFile, failOnUnitMismatch) {
  if (length(logtext) > 0) {
    cat(paste0("# ", length(logtext), " units were automatically corrected.\n"))
    logtext <- paste0("\n\n#--- ", length(logtext), " units were automatically corrected: ---#\n",
          paste0(logtext, collapse = "\n"))
  }

  if (nrow(wrongUnits) > 0) {
    logtext <- c(logtext, reportWrongUnits(wrongUnits))
  }

  if (length(logtext) > 0 && ! is.null(logFile) && ! isFALSE(logFile)) {
    write(logtext, file = logFile, append = TRUE)
  }

  if (failOnUnitMismatch && nrow(wrongUnits) > 0) {
    stop("Unit mismatches!")
  }
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
             "add them to list in piamInterfaces::areUnitsIdentical() as:\n"))
  unitsOnly <- unique(wrongUnits[c(2, 3)])
  for (wno in seq_along(rownames(unitsOnly))) {
    cat(paste0('    c("', unitsOnly[wno, 1], '", "', unitsOnly[wno, 2], '"),\n'))
  }
  cat("\n")
  return(logtext)
}
