#' Checks for a run if it is correctly fixed on the reference run for t < startyear
#'
#' @md
#' @author Oliver Richters
#' @param data quitte object or mif file
#' @param refscen scenario name of reference scenario, or file or quitte object with reference data
#' @param startyear first time step for which scenarios and reference scenario are expected to differ
#' @param ret "boolean": just return TRUE/FALSE if check was successful
#'            "fails": data frame with mismatches between scenario and reference data
#'            "fixed": quitte object with data correctly fixed on reference data
#' @param failfile csv file to which mismatches are written to
#' @importFrom dplyr case_when first group_by summarise ungroup left_join mutate arrange %>%
#'             filter select desc n
#' @importFrom quitte as.quitte quitteSort
#' @importFrom tidyr pivot_wider
#' @importFrom utils write.csv
#' @return see parameter 'ret'
#' @export

fixOnRef <- function(data, refscen, startyear, ret = "boolean", failfile = NULL) {
  scenario <- variable <- period <- value <- ref <- reldiff <- NULL
  data <- droplevels(as.quitte(data, na.rm = TRUE))
  startyear <- suppressWarnings(as.numeric(startyear))
  # check whether refscen is just the scenario name, or rather data
  if (is.character(refscen) && ! file.exists(refscen) && all(refscen %in% levels(data$scenario))) {
    refdata <- droplevels(filter(data, scenario == refscen))
  } else {
    refdata <- droplevels(as.quitte(refscen, na.rm = TRUE))
  }
  refscen <- levels(refdata$scenario)
  stopifnot(
    `'refscen' must be a single scenario only` = length(refscen) == 1,
    `'startyear' must be a single numeric value` = (length(startyear) == 1 && ! is.na(startyear)),
    `'ret' must be 'boolean', 'fails' or 'fixed'` = (length(ret) == 1 && ret %in% c("boolean", "fails", "fixed"))
  )

  returnhelper <- function(ret, boolean, fails, fixed) {
    messages <- c(boolean = "Returning a boolean.",
                  fails   = "Returning failing data.",
                  fixed   = "Returning fixed data.")
    message(messages[[ret]])
    if (ret == "boolean") return(boolean)
    if (ret == "fails") return(fails)
    if (ret == "fixed") return(fixed)
  }

  message("Comparing with reference run ", refscen, " for t < ", startyear)
  if (startyear <= min(data$period)) {
    message("No data before startyear found, so no fixing happened.")
    return(returnhelper(ret, boolean = TRUE, fails = NULL, fixed = data))
  }

  # define false-positives that necessarily differ
  falsepositives <- c(grep("Moving Avg$", levels(data$variable), value = TRUE),
                      grep("Interest Rate (t+1)/(t-1)", levels(data$variable), value = TRUE, fixed = TRUE)
                     )
  # prepare reference data to left_join it to data
  refcomp <- refdata %>%
    select(-scenario) %>%
    rename(ref = value) %>%
    droplevels()
  # left_join data with reference run and select everything with bigger differences
  comp <- data %>%
    left_join(refcomp, by = c("model", "region", "variable", "unit", "period")) %>%
    filter(! variable %in% falsepositives, period < startyear) %>%
    mutate(reldiff = abs(value - ref) / pmax(1E-14, abs(value), abs(ref), na.rm = TRUE)) %>%
    filter(abs(reldiff) > 1E-14) %>%
    droplevels() %>%
    quitteSort()
  if (nrow(comp) == 0) {
    message("\n### All runs are perfectly fixed on reference run!")
    return(returnhelper(ret, boolean = TRUE, fails = NULL, fixed = data))
  }
  # print human-readbable summary
  .printRefDiff(data, comp)
  # save mismatches to file, if requested
  if (! is.null(failfile) && nrow(comp) > 0) {
    message("Find failing variables in '", failfile, "'.")
    write.csv(comp, failfile, quote = FALSE, row.names = FALSE)
  }
  # fix correctly on ref
  fixeddata <- data %>%
    left_join(refcomp, by = c("model", "region", "variable", "unit", "period")) %>%
    mutate(value = case_when(
      period >= startyear ~ value,
      variable %in% falsepositives ~ value,
      is.na(ref) ~ value,
      .default = ref
    )) %>%
    select(-ref) %>%
    quitteSort()
  return(returnhelper(ret, boolean = FALSE, fails = comp, fixed = fixeddata))
}

.printRefDiff <- function(data, comp, groupdepth = 3) {
  .extractvargroup <- function(x, depth) {
    return(unlist(lapply(lapply(str_split(x, "\\|"), head, depth), paste, collapse = "|")))
  }
  model <- scenario <- variable <- period <- reldiff <- group <- NULL
  for (m in levels(data$model)) {
    for (s in levels(data$scenario)) {
      mismatches <- comp %>%
        filter(model == m, scenario == s) %>%
        select(-model, -scenario)
      if (nrow(mismatches) == 0) {
        message("\n### Everything fine for model=", m, " and scenario=", s)
      } else {
        mismatches <- mismatches %>%
          mutate(variable = factor(removePlus(variable))) %>%
          arrange(variable) %>%
          summarise(period = paste(sort(unique(period)), collapse = ","),
                    reldiff = max(reldiff),
                    .by = variable) %>%
          mutate(group = factor(.extractvargroup(variable, groupdepth))) %>%
          summarise(variable = if (length(unique(variable)) == 1) unique(variable) else unique(group),
                    variables = n(),
                    period = paste(sort(unique(strsplit(period, ",")[[1]])), collapse = ", "),
                    reldiff = max(reldiff),
                    .by = group) %>%
          mutate(reldiff = niceround(reldiff), group = variable, variable = NULL) %>%
          droplevels()
        message("\n### Incorrect fixing for these variable groups for model=", m, " and scenario=", s)
        showrows <- 250
        rlang::with_options(width = 160, print(mismatches, n = showrows))
        if (showrows < nrow(mismatches)) {
          message("Further ", (nrow(mismatches) - showrows), " variable groups differ.")
        }
      }
    }
  }
}
