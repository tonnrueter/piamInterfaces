#' Check whether units are identical following a specified list
#'
#' @md
#' @author Oliver Richters
#' @param ... units to be checked
#' @return boolean
#' @export
areUnitsIdentical <- function(...) {
  # only add units that actually have the same meaning, just different spelling
  units <- c(...)
  if (length(units) < 2) stop("Less than two units supplied, cannot check identity.")
  identicalUnits <- list(
    c("°C", "K"),
    c("billion m2/yr", "bn m2/yr"),
    c("billion pkm/yr", "bn pkm/yr"),
    c("billion tkm/yr", "bn tkm/yr"),
    c("billion vkm/yr", "bn vkm/yr"),
    c("Gtkm/yr", "bn tkm/yr"),
    c("Gpkm/yr", "bn pkm/yr"),
    c("kcal/cap/day", "kcal/capita/day"),
    c("km³", "km3"),
    c("kt CF4/yr", "kt CF4-equiv/yr"),
    c("ktU/yr", "kt U/yr"),
    c("Million", "million"),
    c("million t DM/yr", "Mt DM/yr"),
    c("million vehicles", "Million vehicles"),
    c("Mt/yr", "Mt/year"),
    c("Mt CO2-equiv/Mt", "Mt CO2/Mt"),
    c("Mt CO2-equiv/yr", "Mt CO2eq/yr", "Mt CO2e/yr", "Mt CO2/yr"),
    c("Mt Nr/yr", "Tg N/yr"),
    c("Mt NO2/yr", "Mt NOX/yr"),
    c("Percent", "percent", "%"),
    c("unitless", "", "-", "1", "index", "Index (2020 = 1)", "Index (2010 = 1)"),
    c("tDM/cap/yr", "tDM/capita/yr"),
    c("USD05", "USD2005", "US$05", "US$2005", "USD2005", "USD_2005"),
    c("USD10", "USD2010", "US$10", "US$2010", "USD2010", "USD_2010"),
    # below, exceptionally added units that actually differ for backwards compatibility
    # for 'Energy Service|Residential and Commercial|Floor Space'
    c("bn m2/yr", "billion m2/yr", "bn m2", "billion m2"),
    # for 'Productivity|Yield' and subvariables
    c("t DM/ha", "t DM/ha/yr"),
  NULL)
  return(any(unlist(lapply(identicalUnits, function(x) all(c(...) %in% x)))))
}
