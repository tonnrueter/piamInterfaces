#' Checks for a run if the regions for selected variables sum up as expected
#'
#' @md
#' @author Falk Benke
#' @param mifFile path to the mif file to apply summation checks to, or quitte object
#' @param parentRegion region to sum up to. Defaults to World or GLO
#' @param childRegions regions that should sum up to `parentRegion`. Default to all except parentRegion
#' @param variables list of variables to check. Defaults to all in mifFile
#' @param skipUnits units to be skipped. Set to TRUE to get list of units pointing towards
#'        their variable being intensive. You can also use c(TRUE, "additionalunit")
#' @param skipBunkers set to TRUE to skip AR6 variables that contain bunkers only at the global level
#' @param intensiveUnits intensive units where the global value should not be the sum, but instead lie between
#'        the regional values. Set to TRUE to get list of units pointing towards their variable being intensive.
#'        You can also use c(TRUE, "additionalunit").
#' @param absDiff threshold for absolute difference between parent variable and summation
#' @param relDiff threshold (in percent) for relative difference between parent variable and summation
#' @importFrom dplyr group_by summarise ungroup left_join mutate %>% filter select bind_rows
#' @importFrom rlang sym syms .data
#' @importFrom quitte as.quitte
#' @examples
#' \dontrun{
#'checkSummationsRegional(
#'   mifFile = "path/to/file",
#'   childRegions = c("R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF"),
#'   parentRegion = "World",
#'   variables = c("Final Energy|Industry", "Emissions|CO2|Energy|Demand|Industry")
#')
#'}
#' @export
checkSummationsRegional <- function(mifFile, parentRegion = NULL, childRegions = NULL,
                                    variables = NULL, skipUnits = NULL, skipBunkers = NULL, intensiveUnits = TRUE,
                                    absDiff = 0.0001, relDiff = 0.1) {

  # load data
  data <- droplevels(quitte::as.quitte(mifFile, na.rm = TRUE))
  if (! is.null(variables))  data <- droplevels(filter(data, .data$variable %in% variables))
  if (is.null(parentRegion)) parentRegion <- intersect(c("World", "GLO"), levels(data$region))[[1]]
  if (is.null(childRegions)) childRegions <- setdiff(levels(data$region), parentRegion)

  # generate a set of units that are intensive units, that get aggregated not by sum but via (weighted) mean
  years <- paste0(c("05", 2005, 2010, 2015, 2017, 2020))
  index <- c(paste0("Index ", years, "=100"), paste0("Index (", years, " = 1)"))
  unit <- c("", "%", "% of Total GDP", "% pa", "%/yr", "$/GJ", "1", "arbitrary unit", "arbitrary unit/yr",
            "billionDpktU", "billionDpTWyr", "cm/capita", "DM per live animal", "GE per GE",
            "GJ/cap/yr", "GJ/t", "hectares per capita", "index", "Index",
            "kcal/cap/day", "kcal/capita/day", "kcal/kcal", "m3/ha", "MJ/t", "MJ/vehkm",
            "Mt CO2-equiv/EJ", "Mt CO2-equiv/Mt", "Mt CO2/Mt", "Mt CO2/EJ", "Mt Nr/Mt Nr", "Nr per Nr", "percent",
            "Percent", "protein/capita/day", "ratio", "share", "share of total land", "t DM/ha", "t DM/ha/yr",
            "tC/ha", "tC/tC", "tDM/capita/yr", "tDM/cap/yr", "unitless", "years")
  curr <- c("USD", paste0("USDMER", years), paste0("USD", years), paste0("US$", years), paste0("USD_", years),
            paste0("EUR_", years), paste0("EUR", years))
  usecurr <- c("EJ/billion __", "MJ/__", "Mt CO2-equiv/__", "t/million __", "tr __/input unit", "tr__/Input",
               "__ PPP/cap/yr", "k__/per capita", "__/capita", "__/EJ", "__/GJ", "__/h", "__/ha", "__/tDM",
               "__/worker", "__/GJ", "__/kW", "__/kW/yr", "__/t CH4", "__/t CO2", "_/t CO2/yr", "__/t N2O",
               "__/tCH4", "__/tCO2", "__/tCO2 yr", "__/t CO2/yr", "__/tN2O", "__/__", "__/yr", "__/cap/yr")
  unit <- unique(c(index, unit, unlist(lapply(curr, function(x) gsub("__", x, usecurr)))))

  # if intensiveUnits or skipUnits contain 'TRUE', add this list of intensive units to them
  if (TRUE %in% intensiveUnits) {
    intensiveUnits <- unique(c(setdiff(intensiveUnits, TRUE), unit))
  }
  intensiveUnits <- intersect(intensiveUnits, levels(data$unit))
  if (TRUE %in% skipUnits) {
    skipUnits <- c(setdiff(skipUnits, TRUE), unit)
  }
  data <- droplevels(filter(data, ! .data$unit %in% skipUnits))

  # if desired, skip bunker emissions where regional values are not thought to sum to global sums
  if (isTRUE(skipBunkers)) {
    gases <- c("BC", "CO", "CO2", "Kyoto Gases", "NOx", "OC", "Sulfur", "VOC")
    vars <- c("", "|Energy", "|Energy Demand|Transportation", "|Energy and Industrial Processes",
              "|Energy|Demand", "|Energy|Demand|Transportation", "|Transportation")
    gasvars <- expand.grid(gases, vars, stringsAsFactors = FALSE)
    bunkervars <- unique(sort(c("Gross Emissions|CO2", paste0("Emissions|", gasvars$Var1, gasvars$Var2))))
    data <- droplevels(filter(data, ! .data$variable %in% bunkervars))
  }

  # select only parent region
  total <- filter(data, .data$region %in% parentRegion) %>%
    rename("total" = "value") %>%
    select(-"region")

  # for non-intensive units, calculate the sum and check against total
  sumChilds <- filter(data, .data$region %in% childRegions, ! .data$unit %in% intensiveUnits) %>%
    group_by(.data$model, .data$scenario, .data$variable, .data$period, .data$unit) %>%
    summarise(checkSum = sum(.data$value), .groups = "drop") %>%
    ungroup()

  sumCheck <- left_join(sumChilds, filter(total, ! .data$unit %in% intensiveUnits),
                        by = c("model", "scenario", "variable", "period", "unit")) %>%
    mutate(
      diff = .data$checkSum - .data$total,
      reldiff = 100 * (.data$checkSum - .data$total) / .data$total
    ) %>%
    filter(abs(.data$reldiff) >= relDiff, abs(.data$diff) >= absDiff) %>%
    droplevels()

  sumCheckVars <- levels(sumCheck$variable)
  if (length(sumCheckVars) > 0) {
    message("Found summation issues in ", length(sumCheckVars), " variables:\n",
            paste(head(sumCheckVars, 10), collapse = ", "), if (length(sumCheckVars) > 10) " ...")
  }

  # for intensive units, calculate regional min and max and check against total
  minMaxCheck <- filter(data, .data$region %in% childRegions, .data$unit %in% intensiveUnits) %>%
    group_by(.data$model, .data$scenario, .data$variable, .data$period, .data$unit) %>%
    summarise(max = max(.data$value), min = min(.data$value), .groups = "drop") %>%
    left_join(filter(total, .data$unit %in% intensiveUnits),
              by = c("model", "scenario", "variable", "period", "unit")) %>%
    filter(.data$total < .data$min - absDiff | .data$total > .data$max + absDiff) %>%
    droplevels()

  minMaxVars <- sort(levels(minMaxCheck$variable))
  if (length(minMaxVars) > 0) {
    message("Aggregated value lies outside regional min/max for ", length(minMaxVars), " intensive variables:\n",
            paste(head(minMaxVars, 10), collapse = ", "), if (length(minMaxVars) > 10) " ...")
  }

  # bind and return everything
  return(droplevels(bind_rows(sumCheck, minMaxCheck)))
}
