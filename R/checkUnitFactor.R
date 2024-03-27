#' Check unit factor in template
#'
#' @md
#' @author Oliver Richters
#' @param template object provided by loadIIASAtemplate()
#' @param failOnUnitMismatch boolean whether to fail in case of unit mismatches
#'        recommended for submission, not used for checking mapping
#' @param logFile filename of file for logging
#' @importFrom dplyr filter
#' @importFrom rlang sym syms .data
#' @importFrom stringr str_split
#' @return quitte object with adapted mif data
#' @export

checkUnitFactor <- function(template, logFile = NULL, failOnUnitMismatch = TRUE) {
  errortext <- NULL
  colnames(template) <- tolower(names(template))

  # check whether scales are correctly transformed. c(piam_factor, unit, piam_unit)
  # the first line checks that mapping "billion whatever" to "million whatever" uses a factor 1000 etc.
  scaleConversion <- list(
                          c("1", "million", "Million vehicles"),
                          c("1", "Index (2020 = 1)", "1"),
                          c("1", "Index (2010 = 1)", "1"),
                          c("6", "GWh/yr", "GW/yr"), # for 'New Cap|Electricity|Storage|Battery'
                          c("6", "GWh", "GW"),       # for 'Cap|Electricity|Storage|Battery'
                          c("100", "%", "unitless"),
                          c("100", "%", "share of total land"),
                          c("100", "%", "income"),
                          c("1000", "million", "billion"),
                          c("1000", "P", "E"),
                          c("1000", "T", "P"),
                          c("1000", "G", "T"),
                          c("1000", "M", "G"),
                          c("1000", "k", "M"),
                          # conversion factors taken from ECEMF Model Comparison Protocol, DOI:10.5281/zenodo.6811317
                          c("1.12", "US$2010", "US$2005"),
                          c("1.12", "US$2010", "US$05"),
                          c("1.12", "US$2010/t CO2", "US$2005/tCO2"),
                          c("0.00112", "billion US$2010/yr", "million US$05 PPP/yr"),
                          c("0.000892857", "EJ/billion US$2010", "MJ/US$2005"), # 0.001 divided by 1.12
                          c("1.33", "US$2020", "US$2005"),
                          c("1.17", "EUR_2020", "US$2005"),
                          c("1.174", "EUR2020", "US$2005"),
                          # temporary, error in ARIADNE mapping for 'Capital Stock'
                          c("1.174", "billion EUR2020/yr", "billion US$2005")
                         )
  template$piam_factor[is.na(template$piam_factor)] <- 1
  success <- areUnitsIdentical(template$piam_unit, template$unit) & template$piam_factor %in% c(1, -1)
  success <- success | is.na(template$piam_variable) | template$piam_variable %in% c("TODO", "Emi|CO2|Energy|+|Waste")

  firsterror <- TRUE
  for (sc in scaleConversion) {
    fails <- template %>%
               mutate(matches = .data$piam_unit == gsub(sc[[2]], sc[[3]], .data$unit, fixed = TRUE)) %>%
               mutate(matches = .data$matches & grepl(sc[[2]], .data$unit, fixed = TRUE)) %>%
               mutate(matches = .data$matches & ! grepl(paste0("/", sc[[2]]), .data$unit, fixed = TRUE)) %>%
               mutate(failed  = ! .data$piam_factor %in% c(sc[[1]], paste0("-", sc[[1]])))
    wrongScale <- filter(fails, .data$failed & .data$matches)
    if (nrow(wrongScale) > 0) {
      if (isTRUE(firsterror)) {
        errortext <- c(errortext, "According to checkUnitFactor(), the following variables use the wrong piam_factor:")
        firsterror <- FALSE
      }
      errortext <- c(errortext,
        paste0("\n- ", paste0(wrongScale$variable, ": ", wrongScale$piam_factor, collapse = "\n- "),
               "\n  Expected: ", sc[[1]], " ", sc[[2]], " = 1 ", sc[[3]])
      )
    }
    success <- success | (fails$matches & ! fails$failed)
  }

  if (! is.null(logFile) && ! isFALSE(logFile)) {
    dir.create(dirname(logFile), recursive = TRUE, showWarnings = FALSE)
    if (length(errortext) > 0) {
      write(c("\n### checkUnitFactor\n", errortext), file = logFile, append = TRUE)
    } else {
      write("\n### checkUnitFactor finished without error\n", file = logFile, append = TRUE)
    }
  }

  if (length(errortext) > 0) {
    errortext <- c("Conversion factors do not match units!\n", errortext)
    if (failOnUnitMismatch) stop(errortext) else message(errortext)
  }
  return(success)
}
