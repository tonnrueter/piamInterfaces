#' Converts data in historical.mif to match project-specific variables and
#' regions so that it can be used for comparison in an intermodel comparison project
#'
#' @md
#' @author Falk Benke
#' @param mif quitte object with historical data or path to historical.mif
#' @param project name of the project, determines the mapping to be loaded
#' @param regionMapping (optional) csv file with mapping of REMIND regions or ISO to
#'  project regions, must contain two columns:
#'  One can be called 'REMIND' or 'CountryCode' and contains the regions in the data.
#'  The second can be called 'project_region' or 'RegionCode', to which the first is mapped.
#'  You can also specify the filename part before the .csv from inst/regionmapping
#' @param mainReg if regionMapping is specified, additional main region that is kept as is
#' @importFrom dplyr %>% mutate select right_join left_join
#' @importFrom quitte read.quitte as.quitte
#' @importFrom piamutils deletePlus
#' @importFrom stats aggregate
#' @importFrom tidyselect all_of
#' @examples
#' \dontrun{
#' data <- convertHistoricalData(
#'   mif = "path/to/historical.mif",
#'   project = "NAVIGATE",
#'   regionMapping = "path/to/region_mapping_NAVIGATE.csv"
#' )
#' }
#' @export
convertHistoricalData <- function(mif, project, regionMapping = NULL, mainReg = "World") {

  hist <- suppressWarnings(as.quitte(mif, na.rm = TRUE)) %>%
    rename("hist_variable" = "variable", "hist_unit" = "unit")

  m <- NULL

  for (i in project) {
    m <- rbind(m, getMapping(i, requiredColsOnly = TRUE))
  }

  varmap <- m %>%
    filter(!is.na(.data$piam_variable)) %>%
    mutate(
      "piam_factor" = ifelse(is.na(.data$piam_factor), 1, as.numeric(.data$piam_factor)), # nolint
      "piam_variable" = deletePlus(.data$piam_variable) # nolint
    )

  # for each project variable count number REMIND variables mapping to it
  varmap <- left_join(varmap, count(varmap, .data$variable, name = "countRemindVar"), by = c("variable"))

  out <- hist %>%
    left_join(varmap, by = c("hist_variable" = "piam_variable", "hist_unit" = "piam_unit"),
              relationship = "many-to-many") %>%
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
    mappings <- Sys.glob(file.path(system.file("regionmapping", package = "piamInterfaces"), "*.csv"))
    names(mappings) <- gsub(".csv", "", basename(mappings))
    if (regionMapping %in% names(mappings)) regionMapping <- mappings[[regionMapping]]
    regmap <- read.csv2(regionMapping, header = TRUE)
    from <- intersect(names(regmap), c("REMIND", "CountryCode"))[[1]]
    to <- intersect(names(regmap), c("project_region", "RegionCode"))[[1]]
    if (length(from) == 0 || length(to) == 0) {
      stop("regionMapping must contain columns 'REMIND'/'project_region' or 'CountryCode'/'RegionCode'")
    }
    if (length(mainReg) == 1 && ! mainReg %in% regmap[[to]]) {
      rbind(select(regmap, all_of(to), all_of(from)), c(mainReg, mainReg))
    }
    out <- left_join(out, regmap, by = c("region" = from)) %>%
      filter(!is.na(.data[[to]])) %>%
      select("model", "scenario", "region" = all_of(to), "variable", "unit", "period", "value")
  }

  out <- aggregate(value ~ model + scenario + region + period + variable + unit, data = out, FUN = sum)

  return(out)
}
