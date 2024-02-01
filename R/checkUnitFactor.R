#' Check unit factor in template
#'
#' @md
#' @author Oliver Richters
#' @param template object provided by loadIIASAtemplate()
#' @param failOnUnitMismatch boolean whether to fail in case of unit mismatches
#'        recommended for submission, not used for generating templates
#' @param logFile filename of file for logging
#' @importFrom dplyr filter
#' @importFrom rlang sym syms .data
#' @importFrom stringr str_split
#' @return quitte object with adapted mif data
#' @export

checkUnitFactor <- function(template, logFile = NULL, failOnUnitMismatch = TRUE) {
  errortext <- NULL
  colnames(template) <- tolower(names(template))

  # check whether scales are correctly transformed
  # the first line checks that mapping "billion whatever" to "million whatever" uses a factor 1000 etc.
  scaleConversion <- list(
                          c("1000", "million", "billion"),
                          c("1000", "P", "E"),
                          c("1000", "T", "P"),
                          c("1000", "G", "T"),
                          c("1000", "M", "G"),
                          c("1000", "k", "M"),
                          # conversion factors taken from ECEMF Model Comparison Protocol, DOI:10.5281/zenodo.6811317
                          c("1.12", "US$2010", "US$2005"),
                          c("1.33", "US$2020", "US$2005"),
                          c("1.17", "EUR_2020", "US$2005")
                         )
  firsterror <- TRUE
  for (sc in scaleConversion) {
    wrongScale <- template %>%
                    filter(grepl(sc[[2]], .data$unit, fixed = TRUE)) %>%
                    filter(! grepl(paste0("/", sc[[2]]), .data$unit, fixed = TRUE)) %>%
                    filter(.data$piam_unit %in% gsub(sc[[2]], sc[[3]], .data$unit, fixed = TRUE)) %>%
                    filter(! .data$piam_factor %in% c(sc[[1]], paste0("-", sc[[1]])))
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
  }

  if (! is.null(logFile)) {
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
}
