#' Add price index
#'
#' @md
#' @author Oliver Richters
#' @param mifdata file or data that can be converted into quitte object
#' @param priceIndices vector of missing price index variable names
#' @param scenBase optional scenario name of baseline scenario used to calculate index
#' @param referenceYear in which index = 1
#' @export
priceIndicesAdd <- function(mifdata, priceIndices, scenBase = NULL, referenceYear = 2020) {

  mifdata <- quitte::as.quitte(mifdata)

  # price indices
  if (is.null(scenBase)) {
    dataForReferenceYear <- mifdata %>%
        filter(!!sym("period") == referenceYear) %>%
        select(-!!sym("period")) %>%
        rename(value_ref = !!sym("value"))
    joinby <- c("model", "region", "variable", "unit", "scenario")
  } else {
    dataForReferenceYear <- mifdata %>%
        filter(!!sym("period") == referenceYear, grepl(scenBase, !!sym("scenario"))) %>%
        select(-!!sym("period"), -!!sym("scenario")) %>%
        rename(value_ref = !!sym("value"))
    joinby <- c("model", "region", "variable", "unit")
  }

  priceVariables <- gsub("\\|Index$", "", priceIndices)
  priceIndexData <- filter(mifdata, !!sym("variable") %in% priceVariables) %>%
      left_join(dataForReferenceYear, by = joinby, relationship = "many-to-many") %>%
      mutate(value = !!sym("value") / !!sym("value_ref")) %>%
      mutate(variable = paste0(!!sym("variable"), "|Index")) %>%
      mutate(unit = paste0("Index (", referenceYear, " = 1)")) %>%
      select(-!!sym("value_ref"))

  mifdata <- rbind(mifdata, priceIndexData)
  return(mifdata)
}
