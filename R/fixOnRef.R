#' Checks for a run if it is correctly fixed on the reference run for t < startyear
#'
#' @md
#' @author Oliver Richters
#' @param mif data or path to mif file of single scenario
#' @param mifRef data or path to mif file of reference scenario
#' @param startyear first time step for each mif and mifRef are expected to differ
#' @param ret "boolean", "fixed" or "fails", depending on what you want to get
#' @param failfile csv file to which failing check are written to
#' @importFrom dplyr group_by summarise ungroup left_join mutate arrange %>%
#'             filter select desc n
#' @importFrom gms getLine
#' @importFrom quitte as.quitte
#' @importFrom tidyr pivot_wider
#' @importFrom utils write.csv
#'
#' @export


fixOnRef <- function(mif, mifRef, startyear, ret = "boolean", failfile = NULL) {
  scenario <- variable <- period <- value <- ref <- reldiff <- group <- NULL
  mif <- droplevels(as.quitte(mif, na.rm = TRUE))
  mifRef <- droplevels(as.quitte(mifRef, na.rm = TRUE))
  startyear <- suppressWarnings(as.numeric(startyear))
  stopifnot(
    `'mif' must contain data from one scenario only` = length(levels(mif$scenario)) == 1,
    `'mifRef' must contain data from one scenario only` = length(levels(mifRef$scenario)) == 1,
    `'startyear' must be a single numeric value` = (length(startyear) == 1 && ! is.na(startyear)),
    `'ret' must be 'boolean', 'fails' or 'fixed'` = (length(ret) == 1 && ret %in% c("boolean", "fails", "fixed"))
  )

  title <- levels(mif$scenario)
  titleRef <- levels(mifRef$scenario)

  if (identical(levels(mif$scenario), levels(mifRef$scenario))) {
    levels(mifRef$scenario) <- paste0(levels(mifRef$scenario), "_ref")
  }

  falsepositives <- grep("Moving Avg$", levels(mif$variable), value = TRUE)

  message("Comparing ", title, " with reference run ", titleRef, " for t < ", startyear)

  if (startyear <= min(mif$period)) {
    message("No data before startyear found, so no fixing happened")
    return(if (ret == "fails") NULL else if (ret == "fixed") mif else TRUE)
  }
  comp <- rbind(mutate(mif, scenario = "value"), mutate(mifRef, scenario = "ref")) %>%
    filter(! variable %in% falsepositives, period < startyear) %>%
    arrange(variable) %>%
    pivot_wider(names_from = scenario) %>%
    mutate(reldiff = abs(value - ref) / pmax(1E-14, abs(value), abs(ref), na.rm = TRUE)) %>%
    filter(abs(reldiff) > 1E-14) %>%
    mutate(scenario = factor(title)) %>%
    droplevels()
  if (nrow(comp) == 0) {
    message("# Run is perfectly fixed on reference run!")
    return(if (ret == "fails") NULL else if (ret == "fixed") mif else TRUE)
  }
  mismatches <- comp %>%
    mutate(model = NULL, scenario = NULL, variable = factor(removePlus(variable))) %>%
    arrange(variable) %>%
    summarise(period = paste(sort(unique(period)), collapse = ","),
              reldiff = max(reldiff),
              .by = variable) %>%
    mutate(group = factor(gsub("(\\|.*?)\\|.*$", "\\1", variable))) %>%
    # mutate(group = factor(gsub("\\|.*", "", variable))) %>% # to group more coarsely
    summarise(variable = if (length(unique(variable)) == 1) unique(variable) else unique(group),
              variables = n(),
              period = paste(sort(unique(strsplit(period, ",")[[1]])), collapse = ", "),
              reldiff = max(reldiff),
              .by = group) %>%
    mutate(reldiff = niceround(reldiff), group = variable, variable = NULL) %>%
    droplevels()

  showrows <- 250
  rlang::with_options(width = 160, print(mismatches, n = showrows))
  if (showrows < nrow(mismatches)) {
    message("Further ", (nrow(mismatches) - showrows), " variable groups differ.")
  }
  if (! is.null(failfile) && nrow(comp) > 0) {
    message("Find failing variables in '", failfile, "'.")
    write.csv(comp, failfile, quote = FALSE, row.names = FALSE)
  }
  if (ret %in% c("fails", "boolean")) {
    return(if (ret == "boolean") FALSE else comp)
  }
  message("Returning corrected data for ", title, ".")
  di <- rbind(
            filter(mif, period >= startyear | ! variable %in% levels(mifRef$variable) | variable %in% falsepositives),
            filter(mifRef, period < startyear, variable %in% setdiff(levels(mif$variable), falsepositives))
           ) %>%
        mutate(scenario = factor(title)) %>%
        quitteSort()
  return(di)
}
