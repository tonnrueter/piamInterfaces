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
  abbreviations <- list(
    "bn" = "billion",
    "US" = "US\\$|USD_|USD|US_",
    "US05" = "US2005",
    "US10" = "US2010",
    "EUR" = "EUR_",
    "yr" = "year",
    "mio" = "million|Million",
    "CO2" = "CO2e|CO2eq|CO2-equiv",
    "%" = "Percentage|Percent|percent",
    "cap" = "capita"
  )
  identicalUnits <- list(
    c("\u00B0C", "\u00C2\u00B0C", "K"),
    c("Gtkm/yr", "bn tkm/yr"),
    c("Gpkm/yr", "bn pkm/yr"),
    c("kcal/cap/day", "kcal/capita/day", "kcal/cap/d"),
    c("kt CF4/yr", "kt CF4-equiv/yr"),
    c("ktU/yr", "kt U/yr"),
    c("km\u00b3", "km3"),
    c("mio", "mio people"),
    c("million t DM/yr", "Mt DM/yr"),
    c("Mt Nr/yr", "Tg N/yr"),
    c("Mt NO2/yr", "Mt NOX/yr"),
    c("Nr/Nr", "Nr per Nr"),
    c("unitless", "", "-", "1", "index"),
    c("W/m2", "W/m^2"),
    # below, exceptionally added units that actually differ for backwards compatibility
    # for 'Energy Service|Residential and Commercial|Floor Space'
    c("bn m2/yr", "billion m2/yr", "bn m2", "billion m2"),
    # for 'Productivity|Yield' and subvariables
    c("t DM/ha", "t DM/ha/yr", "dm t/ha"),
    # for 'Water|Environmental flow violation volume'
    c("km3/yr", "km3"),
  NULL)
  areIdentical <- function(x, y) {
    # literally identical
    isTRUE(x == y) ||
    # both found in the same list element above
    any(unlist(lapply(identicalUnits, function(units) all(c(x, y) %in% units))))
  }
  for (abb in names(abbreviations)) {
    vec1 <- gsub(abbreviations[abb], abb, vec1)
    vec2 <- gsub(abbreviations[abb], abb, vec2)
    identicalUnits <- lapply(identicalUnits, function(x) gsub(abbreviations[abb], abb, x))
  }
  return(unname(unlist(Map(Vectorize(areIdentical), vec1, vec2))))
}
