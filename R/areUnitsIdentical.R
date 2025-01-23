#' Check whether units are identical following a specified list
#'
#' @md
#' @author Oliver Richters
#' @param vec1 units to be checked against vec2, elementwise
#' @param vec2 units to be checked against vec1, elementwise
#' @return boolean
#' @export
areUnitsIdentical <- function(vec1, vec2 = NULL) {
  if (is.null(vec2)) vec2 <- head(vec1, n = 1)
  # add abbreviations here that are used in the units.
  abbreviations <- list(
    "G" = "billion ?|bn ?",
    "M" = "million ?|Million ?|mio ?",
    "US" = "US\\$|USD_|USD|US_",
    "US05" = "US2005",
    "US10" = "US2010",
    "EUR" = "EUR_",
    "yr" = "year",
    "CO2" = "CO2e|CO2eq|CO2-equiv",
    "CF4" = "CF4-equiv",
    "%" = "Percentage|Percent|percent",
    "cap" = "capita",
    "/" = " per "
  )
  # only add units that actually have the same meaning, just different spelling
  identicalUnits <- list(
    c("\u00B0C", "\u00C2\u00B0C", "K"),
    c("kcal/cap/day", "kcal/cap/d"),
    c("ktU/yr", "kt U/yr"),
    c("km\u00b3", "km3"),
    c("mio", "mio people"),
    c("Mt Nr/yr", "Tg N/yr"),
    c("Mt NO2/yr", "Mt NOX/yr"),
    c("unitless", "", "-", "1", "index", NA),
    c("W/m2", "W/m^2"),
    c("million vehicles", "million veh"),
    # below, exceptionally added units that actually differ for backwards compatibility
    # for 'Energy Service|Residential and Commercial|Floor Space'
    c("bn m2/yr", "bn m2"),
    # for 'Productivity|Yield' and subvariables
    c("t DM/ha", "t DM/ha/yr", "dm t/ha"),
    # for 'Water|Environmental flow violation volume'
    c("km3/yr", "km3"),
    c("mha per yr", "million ha/yr"),
  NULL)

  # function to apply abbreviations
  .abbreviateUnit <- function(unit) {
    for (abb in names(abbreviations)) {
      unit <- gsub(abbreviations[abb], abb, unit)
    }
    return(unit)
  }

  # apply abbreviations to inputs and identicalUnits
  vec1 <- .abbreviateUnit(vec1)
  vec2 <- .abbreviateUnit(vec2)
  identicalUnits <- lapply(identicalUnits, .abbreviateUnit)

  # function to check identity
  .areIdentical <- function(x, y) {
    # literally identical
    isTRUE(x == y) ||
    # both found in the same list element of identicalUnits above
    any(unlist(lapply(identicalUnits, function(units) all(c(x, y) %in% units))))
  }

  # check identity on vectors
  return(unname(unlist(Map(Vectorize(.areIdentical), vec1, vec2))))
}
