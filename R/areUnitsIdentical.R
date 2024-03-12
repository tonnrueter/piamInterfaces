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
    c("billion m2/yr", "bn m2/yr"),
    c("billion pkm/yr", "bn pkm/yr"),
    c("billion tkm/yr", "bn tkm/yr"),
    c("billion vkm/yr", "bn vkm/yr"),
    c("kt CF4/yr", "kt CF4-equiv/yr"),
    c("Million", "million"),
    c("Mt/yr", "Mt/year"),
    c("unitless", ""),
    c("million vehicles", "Million vehicles"),
    c("Percent", "percent", "%"),
    c("USD05", "USD2005", "US$05", "US$2005", "USD2005", "USD_2005"),
    c("USD10", "USD2010", "US$10", "US$2010", "USD2010", "USD_2010"),
    # below, exceptionally added units that actually differ for backwards compatibility
    # with AR6 and SHAPE templates (using old incorrect unit)
    # should only affect template variable 'Energy Service|Residential and Commercial|Floor Space'
    c("bn m2/yr", "billion m2/yr", "bn m2", "billion m2"),
  NULL)
  return(any(unlist(lapply(identicalUnits, function(x) all(c(...) %in% x)))))
}
