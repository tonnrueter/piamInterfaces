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
                          c("1000", "MJ", "GJ"),
                          c("1000", "kt", "Mt")
                         )
  wrongScale <- NULL
  for (sc in scaleConversion) {
    wrongScale <- template %>%
                    filter(grepl(sc[[2]], .data$unit)) %>%
                    filter(.data$piam_unit %in% gsub(sc[[2]], sc[[3]], .data$unit, fixed = TRUE)) %>%
                    filter(! .data$piam_factor == sc[[1]]) %>%
                    rbind(wrongScale)
  }

  if (nrow(wrongScale) > 0) {
    errortext <- c(errortext,
                   paste0("The following variables have the wrong factor as scale correction:\n- ",
                          paste0(wrongScale$variable, ": ", wrongScale$piam_factor, collapse = "\n- ")),
                   paste0("The following is expected:",
                          lapply(scaleConversion, function(x) paste0("\n- ", x[[1]], " ", x[[2]], " = 1 ", x[[3]])))
                   )
  }

  # check whether US$2005 values are correctly transformed in US$2010
  wrongInflation <- template %>%
    filter(grepl("US$2010", .data$unit, fixed = TRUE)) %>%
    filter(! grepl("/US$2010", .data$unit, fixed = TRUE)) %>%
    filter(.data$piam_unit %in% gsub("US$2010", "US$2005", .data$unit, fixed = TRUE)) %>%
    filter(! (.data$piam_factor == "1.10774" | (.data$piam_factor == "-1.10774" & grepl("Loss|Policy Cost", .data$variable))))
  if (nrow(wrongInflation) > 0) {
    errortext <- c(errortext,
                   paste0("Those variables should use 1.10774 as inflation correction US$2005 -> US$2010:\n- ",
                          paste0(wrongInflation$variable, ": ", wrongInflation$piam_factor, collapse = "\n- ")))
  }

  if (! is.null(logFile)) {
    if (! dir.exists(dirname(logFile))) dir.create(dirname(logFile), recurse = TRUE)
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
