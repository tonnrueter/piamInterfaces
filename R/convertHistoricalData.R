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

  hist <- suppressWarnings(as.quitte(mif)) %>%
    filter(!is.na(!!sym("value")))

  m <- NULL
  for (i in project) {
    m <- rbind(m, getTemplate(i))
  }

  varmap <- m %>%
    filter(!is.na(!!sym("piam_variable"))) %>%
    mutate(
      !!sym("piam_factor") := ifelse(is.na(!!sym("piam_factor")), 1, as.numeric(!!sym("piam_factor"))), # nolint
      !!sym("piam_variable") := sub("\\|\\++\\|", "|", !!sym("piam_variable")) # nolint
    )

  # count REMIND variables mapping to each project variables
  varmap <- left_join(varmap, count(varmap, !!sym("Variable"), name = "countRemindVar"),
                      by = c("Variable"))

  out <- hist %>%
    left_join(varmap, by = c("variable" = "piam_variable", "unit" = "piam_unit")) %>%
    mutate(!!sym("value") := !!sym("piam_factor") * !!sym("value")) %>%
    select("model", "scenario", "region", "variable" = "Variable", "unit" = "Unit",
           "period", "value", "countRemindVar") %>%
    filter(!is.na(!!sym("value")))

  # filter entries where all REMIND variables used for calculating project variables
  # were found in historical.mif
  complete <- count(out,
                    !!!syms(c("model", "scenario", "region", "variable", "unit", "period", "countRemindVar")),
                    name = "actualRemindVar") %>%
    filter(!!sym("actualRemindVar") == !!sym("countRemindVar"))

  out <- left_join(complete, out,
                   by = c("model", "scenario", "region", "variable", "unit", "period", "countRemindVar")) %>%
    select(-c("actualRemindVar", "countRemindVar"))

  if (!is.null(regionMapping)) {
    regmap <- read.csv2(regionMapping, header = TRUE)

    if (!all(c("REMIND", "project_region") %in% names(regmap))) {
      stop("regionMapping must contain columns 'REMIND' and 'project_region'")
    }

    out <- left_join(out, regmap, by = c("region" = "REMIND")) %>%
      filter(!is.na(!!sym("project_region"))) %>%
      select("model", "scenario", "region" = "project_region", "variable", "unit", "period", "value")
  }

  out <- aggregate(value ~ model + scenario + region + period + variable + unit, data = out, FUN = sum)

  return(out)
}
