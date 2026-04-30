#' Fixes price indices with wrong reference year
#'
#' @md
#' @author Oliver Richters
#' @param mifdata file or data that can be converted into quitte object
#' @param priceIndices vector of missing price index variable names
#' @param referenceYear in which index = 1
#' @export
priceIndicesFix <- function(mifdata, priceIndices, referenceYear = 2020) {

  mifdata <- quitte::as.quitte(mifdata)

  dataForReferenceYear <- mifdata %>%
    filter(!!sym("period") == referenceYear & !!sym("variable") %in% priceIndices) %>%
    select(-!!sym("period")) %>%
    rename(value_ref = !!sym("value"))
  joinby <- c("model", "region", "variable", "unit", "scenario")

  priceIndexData <- filter(mifdata, !!sym("variable") %in% priceIndices) %>%
    left_join(dataForReferenceYear, by = joinby, relationship = "many-to-many") %>%
    mutate(value = !!sym("value") / !!sym("value_ref")) %>%
    mutate(unit = paste0("Index (", referenceYear, " = 1)")) %>%
    select(-!!sym("value_ref"))

  mifdata <- rbind(filter(mifdata, !(!!sym("variable") %in% priceIndices)), priceIndexData)
  return(droplevels(mifdata))
}
