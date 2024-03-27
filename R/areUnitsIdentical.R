#' Check whether units are identical following a specified list
#'
#' @md
#' @author Oliver Richters
#' @param vec1 units to be checked against vec2, elementwise
#' @param vec2 units to be checked against vec1, elementwise
#' @return boolean
#' @export
areUnitsIdentical <- function(vec1, vec2) {
  # only add units that actually have the same meaning, just different spelling
  identicalUnits <- list(
    c("\u00B0C", "K"),
    c("billion m2/yr", "bn m2/yr"),
    c("billion pkm/yr", "bn pkm/yr"),
    c("billion tkm/yr", "bn tkm/yr"),
    c("billion vkm/yr", "bn vkm/yr"),
    c("Gtkm/yr", "bn tkm/yr"),
    c("Gpkm/yr", "bn pkm/yr"),
    c("kcal/cap/day", "kcal/capita/day"),
    c("kt CF4/yr", "kt CF4-equiv/yr"),
    c("ktU/yr", "kt U/yr"),
    c("km\u00b3", "km3"),
    c("Million", "million", "million people"),
    c("million t DM/yr", "Mt DM/yr"),
    c("million vehicles", "Million vehicles"),
    c("Mt/yr", "Mt/year"),
    c("Mt CO2-equiv/Mt", "Mt CO2/Mt"),
    c("Mt CO2-equiv/yr", "Mt CO2eq/yr", "Mt CO2e/yr", "Mt CO2/yr"),
    c("Mt Nr/yr", "Tg N/yr"),
    c("Mt NO2/yr", "Mt NOX/yr"),
    c("Percentage", "Percent", "percent", "%"),
    c("unitless", "", "-", "1", "index"),
    c("tDM/cap/yr", "tDM/capita/yr"),
    c("USD05", "USD2005", "US$05", "US$2005", "USD2005", "USD_2005"),
    c("USD10", "USD2010", "US$10", "US$2010", "USD2010", "USD_2010"),
    # below, exceptionally added units that actually differ for backwards compatibility
    # for 'Energy Service|Residential and Commercial|Floor Space'
    c("bn m2/yr", "billion m2/yr", "bn m2", "billion m2"),
    # for 'Productivity|Yield' and subvariables
    c("t DM/ha", "t DM/ha/yr"),
    # for 'Water|Environmental flow violation volume'
    c("km3/yr", "km3"),
  NULL)
  areIdentical <- function(x, y) {
    # literally identical or both found in the same list element above
    x == y || any(unlist(lapply(identicalUnits, function(units) all(c(x, y) %in% units))))
  }
  return(unname(unlist(Map(Vectorize(areIdentical), vec1, vec2))))
}
