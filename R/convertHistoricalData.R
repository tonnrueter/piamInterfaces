#' Converts data in historical.mif to match project-specific variables and
#' regions so that it can be used for comparison in an intermodel comparison project
#'
#' @md
#' @author Falk Benke
#' @param mif quitte object with historical data or path to historical.mif
#' @param project name of the project, determines the mapping template to be loaded
#' @param regionMapping (optional) csv file with mapping of REMIND regions to
#'  project regions, must contain two columns 'REMIND' and 'project_region'
#' @importFrom dplyr %>% mutate select right_join left_join
#' @importFrom quitte read.quitte as.quitte
#' @importFrom stats aggregate
#' @examples
#' \dontrun{
#' data <- convertHistoricalData(
#'   mif = "path/to/historical.mif",
#'   project = "NAVIGATE",
#'   regionMapping = "path/to/region_mapping_NAVIGATE.csv"
#' )
#' }
#' @export
convertHistoricalData <- function(mif, project, regionMapping = NULL) {

  hist <- suppressWarnings(as.quitte(mif, na.rm = TRUE))

  m <- NULL
  for (i in project) {
    m <- rbind(m, getTemplate(i))
  }

  varmap <- m %>%
    filter(!is.na(.data$piam_variable)) %>%
    mutate(
      "piam_factor" = ifelse(is.na(.data$piam_factor), 1, as.numeric(.data$piam_factor)), # nolint
      "piam_variable" = sub("\\|\\++\\|", "|", .data$piam_variable) # nolint
    )

  # for each project variable count number REMIND variables mapping to it
  varmap <- left_join(varmap, count(varmap, .data$variable, name = "countRemindVar"), by = c("variable"))

  out <- hist %>%
    left_join(varmap, by = c("variable" = "piam_variable", "unit" = "piam_unit")) %>%
    mutate("value" = .data$piam_factor * .data$value) %>%
    select("model", "scenario", "region", "variable", "unit",
           "period", "value", "countRemindVar")

  # filter entries where all REMIND variables used for calculating project variables
  # were found in historical.mif
  complete <- count(out, !!!syms(c("model", "scenario", "region", "variable",
                                   "unit", "period", "countRemindVar")),
                    name = "actualRemindVar") %>%
    filter(.data$actualRemindVar == .data$countRemindVar)

  out <- left_join(complete, out, by = c("model", "scenario", "region", "variable",
                                         "unit", "period", "countRemindVar")) %>%
    select(-c("actualRemindVar", "countRemindVar"))

  if (!is.null(regionMapping)) {
    regmap <- read.csv2(regionMapping, header = TRUE)

    if (!all(c("REMIND", "project_region") %in% names(regmap))) {
      stop("regionMapping must contain columns 'REMIND' and 'project_region'")
    }

    out <- left_join(out, regmap, by = c("region" = "REMIND")) %>%
      filter(!is.na(.data$project_region)) %>%
      select("model", "scenario", "region" = "project_region", "variable", "unit", "period", "value")
  }

  out <- aggregate(value ~ model + scenario + region + period + variable + unit, data = out, FUN = sum)

  return(out)
}
