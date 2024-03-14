#' Checks for a run if the regions for selected variables sum up as expected
#'
#' @md
#' @author Falk Benke
#' @param mifFile path to the mif file to apply summation checks to, or quitte object
#' @param parentRegion region to sum up to. Defaults to World or GLO
#' @param childRegions regions that should sum up to `parentRegion`. Default to all except parentRegion
#' @param variables list of variables to check. Defaults to all in mifFile
#' @param skipUnits units to be skipped, because they are not expected to sum up. Set to TRUE to get
#'        list of units pointing towards their variable being intensive. You can also use c(TRUE, "additionalunit")
#' @importFrom dplyr group_by summarise ungroup left_join mutate %>% filter select
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
                                    variables = NULL, skipUnits = NULL) {
  if (TRUE %in% skipUnits) {
    tmp <- c("", "%", "% of Total GDP", "% pa", "%/yr", "$/GJ", "1", "arbitrary unit", "arbitrary unit/yr",
             "billionDpktU", "billionDpTWyr", "cm/capita", "DM per live animal", "GE per GE",
             "GJ/cap/yr", "GJ/t", "hectares per capita",
             "index", "Index", "Index 2005=100", "Index 2010=100",
             "kcal/cap/day", "kcal/capita/day", "kcal/kcal", "m3/ha", "MJ/t",
             "Mt CO2-equiv/EJ", "Mt CO2/EJ", "Nr per Nr", "percent",
             "Percent", "ratio", "share", "share of total land", "t DM/ha", "t DM/ha/yr",
             "tC/ha", "tC/tC", "tDM/capita/yr", "unitless", "years")
    curr <- c("USD", "USD05", "US$05", "US$2005", "US$2010", "USD_2010", "EUR_2020")
    usecurr <- c("EJ/billion __", "MJ/__", "Mt CO2-equiv/__", "t/million __", "tr __/input unit", "tr__/Input",
                 "__ PPP/cap/yr", "k__/per capita", "__/capita", "__/GJ", "__/ha", "__/tDM",
                 "__/worker", "__/GJ", "__/kW", "__/kW/yr", "__/t CH4", "__/t CO2", "__/t N2O",
                 "__/tCH4", "__/tCO2", "__/tCO2 yr", "__/tN2O", "__/__", "__/yr", "__/cap/yr")
    tmp <- unique(c(tmp, unlist(lapply(curr, function(x) gsub("__", x, usecurr)))))
    skipUnits <- c(setdiff(skipUnits, TRUE), tmp)
  }

  data <- droplevels(quitte::as.quitte(mifFile, na.rm = TRUE))
  if (! is.null(variables))  data <- droplevels(filter(data, .data$variable %in% variables))
  skippedUnits <- intersect(skipUnits, levels(data$unit))
  if (length(skippedUnits) > 0) {
    data <- droplevels(filter(data, ! .data$unit %in% skippedUnits))
    message(length(skippedUnits), " units skipped, as they point to intensive variables that would not sum up anyway.")
  }
  if (is.null(parentRegion)) parentRegion <- intersect(c("World", "GLO"), levels(data$region))[[1]]
  if (is.null(childRegions)) childRegions <- setdiff(levels(data$region), parentRegion)

  sum <- filter(data, !!sym("region") %in% childRegions)  %>%
    group_by(!!!syms(c("model", "scenario", "variable", "period", "unit"))) %>%
    summarise(checkSum = sum(!!sym("value")), .groups = "drop") %>%
    ungroup()

  total <- filter(data, !!sym("region") == parentRegion) %>%
    rename("total" = "value") %>%
    select(-"region")

  tmp <- left_join(sum, total, by = c("model", "scenario", "variable", "period", "unit")) %>%
    mutate(
      diff = !!sym("checkSum") - !!sym("total"),
      reldiff = 100 * (!!sym("checkSum") - !!sym("total")) / !!sym("total")
    )

  return(tmp)
}
