#' Check unit factor in template
#'
#' This function checks whether the piam_factor in a mapping fits unit and piam_unit.
#' It does the following:
#' 1. check whether the units are identical based on areUnitsIdentical() and piam_factor is 1 or -1.
#' 2. based on scaleConversion defined below, check whether manually added factors are satisfied
#'    This works based on regex matching, so for example 1000 TW = 1 PW is matched by the c("1000", "T", "P") line.
#' If the tests fail because of a new unit, you can add them below. If the units are really identical except for
#' spelling, better add them to areUnitsIdentical.R
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

  # if units are really identical, better add them to areUnitsIdentical.R
  # check whether scales are correctly transformed. c(piam_factor, unit, piam_unit)
  # For example, line 7 check that mapping "billion whatever" to "million whatever" uses a factor 1000 etc.
  # This uses regex matching, so "1000 million US$" = "1 billion US$" is covered by that as well.
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
                          c("1.12", "USD_2010", "US$2005"),
                          c("1.12", "US$2010/t CO2", "US$2005/tCO2"),
                          c("0.00112", "billion US$2010/yr", "million US$05 PPP/yr"),
                          c("0.000892857", "EJ/billion US$2010", "MJ/US$2005"), # 0.001 divided by 1.12
                          c("1.33", "US$2020", "US$2005"),
                          c("1.17", "EUR_2020", "US$2005"), # ESABCC
                          c("1.174", "EUR2020", "US$2005"), # ARIADNE
                          # converted aboves numbers using GDPuc
                          c("0.9096", "US$2010", "US$2017"),
                          c("0.9096", "US$2010", "US$17"),
                          c("0.9096", "US$2010/t CO2", "US$2017/tCO2"),
                          c("0.9096", "USD_2010", "US$2017"),
                          c("0.0009096", "billion US$2010/yr", "million US$2017 PPP/yr"),
                          c("0.001099", "EJ/billion US$2010", "MJ/US$2017"), # 0.001 divided by 0.9096
                          c("0.001099", "EJ/billion USD_2010", "MJ/US$2017"), # 0.001 divided by 0.9096
                          c("0.9502", "EUR_2020", "US$2017"), # ESABCC
                          c("0.9534", "EUR2020", "US$2017"),  # ARIADNE
                          # c("0.9311", "EUR2020", "US$2017"),  # GDPuc 0.931132
                          c("0.8121", "USD05", "US$2017"),
                          # temporary, error in ARIADNE mapping for 'Capital Stock'
                          c("1.174", "billion EUR2020/yr", "billion US$2005"),
                          c("0.9534", "billion EUR2020/yr", "billion US$2017"),
                          # AgMIP
                          c("1000", "1000 ha", "million ha"),
                          c("1", "MtCO2", "Mt CO2/yr"),
                          c("1000", "MtCO2e", "Gt CO2e/yr"),
                          c("1", "MtCO2e", "Mt CO2e/yr"),
                          c("1000", "1000 t", "Mt Nr/yr"),
                          c("1", "TgN/year", "Mt Nr/yr"),
                          c("0.8121", "USDMER05", "US$2017"),
                          c("0.0008121", "bn USD 2005 MER", "million US$2017 MER/yr"),
                          c("1000", "1000 t dm", "Mt DM/yr")
                         )
  template$piam_factor[is.na(template$piam_factor)] <- 1
  success <- areUnitsIdentical(template$piam_unit, template$unit) & template$piam_factor %in% c(1, -1)
  success <- success | is.na(template$piam_variable)

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
        paste0("\n- ", paste0(wrongScale$variable, ": ", wrongScale$piam_factor, " ",
                              wrongScale$unit, " = 1 ", wrongScale$piam_unit, collapse = "\n- "),
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
