#' Check NGFS submission by comparing mif data to a template file (xlsx or yaml) provided by IIASA
#'
#' @md
#' @author Oliver Richters
#' @param mifdata quitte object or filename of mif file
#' @param iiasatemplate filename of xlsx or yaml file provided by IIASA
#' @param logFile filename of file for logging. Set to NULL for stdout, set to FALSE for none.
#' @param generatePlots boolean whether to plot failing summations
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom rlang .data
#' @return quitte object with adapted mif data
#' @export
checkNGFS <- function(mifdata, iiasatemplate, logFile, generatePlots = TRUE) {
  fileprefix <- gsub("\\.[A-Za-z]+$", "", logFile)
  periods <- seq(2005, 2100, 5)
  d <- as.quitte(mifdata) %>%
    filter(.data$period %in% periods) %>%
    priceIndicesIIASA(iiasatemplate, scenBase = NULL) %>%
    checkIIASASubmission(iiasatemplate, logFile = NULL, failOnUnitMismatch = FALSE)

  # fix correctly on reference scenario
  fixOnRefNGFS <- function(scen, d) {
    if (scen %in% "h_pol") return(d)
    startyear <- if (grepl("d_delfrag|d_strain", scen)) 2035 else 2025
    d %>%
      filter(.data$scenario %in% c(scen, "h_cpol")) %>%
      fixOnRef("h_cpol", startyear, ret = "fixed", failfile = NULL, relDiff = 1E-12) %>%
      filter(.data$scenario %in% scen) %>%
      return()
  }

  d <- as.quitte(lapply(levels(d$scenario), fixOnRefNGFS, d))

  invisible(checkSummations(d, template = NULL, summationsFile = "AR6", logFile = NULL, logAppend = TRUE,
                            outputDirectory = ".", generatePlots = generatePlots,
                            dataDumpFile = paste0(fileprefix, "_checkSummations.csv"),
                            plotprefix = paste0(fileprefix, "_")))
  csregi <- d %>%
    checkSummationsRegional(skipUnits = TRUE, skipBunkers = TRUE) %>%
    rename(World = "total") %>%
    droplevels()
  checkyear <- 2050
  failregi <- csregi %>%
    filter(abs(.data$reldiff) > 0.5, abs(.data$diff) > 0.00015, .data$period == checkyear) %>%
    # filter(! .data$variable %in% bunkervars) %>%
    select(-"model", -"period")
  if (nrow(failregi) > 0) {
    message("For those variables, the sum of regional values does not match the World value in 2050:")
    failregi %>% niceround() %>% print(n = 1000)
  } else {
    message("Regional summation checks are fine.")
  }

  return(d)
}
