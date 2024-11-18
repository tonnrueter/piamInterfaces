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
  # add abbreviations here that are used in the units
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
  # only add units that actually have the same meaning, just different spelling
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
    c("million vehicles", "million veh"),
    # below, exceptionally added units that actually differ for backwards compatibility
    # for 'Energy Service|Residential and Commercial|Floor Space'
    c("bn m2/yr", "bn m2"),
    # for 'Productivity|Yield' and subvariables
    c("t DM/ha", "t DM/ha/yr", "dm t/ha"),
    # for 'Water|Environmental flow violation volume'
    c("km3/yr", "km3"),
    c("Mm3/yr", "million m3/yr"),
    c("Mha/yr", "million ha/yr"),
    c("mha per yr", "million ha/yr"),
    c(NA, "-"),
  NULL)

  # function to apply abbreviations
  .abbreviateUnit <- function(unit) {
    for (abb in names(abbreviations)) {
      unit <- gsub(abbreviations[abb], abb, unit)
    }
    return(unit)
  }

  # apply abbreviations
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
