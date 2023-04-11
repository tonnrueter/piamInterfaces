#' Add Price|*|Index variables requested in iiasatemplate but missing in data,
#' if Price|* is present in data. Extracts reference year automatically from unit
#'
#' @md
#' @author Oliver Richters
#' @param mifdata file or data that can be converted into quitte object
#' @param iiasatemplate filename of xlsx or yaml file provided by IIASA
#' @param scenBase optional scenario name of baseline scenario used to calculate index
#' @export
priceIndicesIIASA <- function(mifdata, iiasatemplate, scenBase = NULL) {
  # Add missing Price|*|Index variables if Price|* is present in data
  template <- loadIIASATemplate(iiasatemplate)
  expectedPriceIndices <- grep("^Price\\|.*\\|Index$", template$variable, value = TRUE)
  missingPriceIndices <- setdiff(expectedPriceIndices, levels(mifdata$variable))
  for (mpi in missingPriceIndices) {
    templateunit <- unique(template$unit[template$variable == mpi])
    referenceYear <- extractReferenceYear(templateunit)
    mifdata <- priceIndicesAdd(mifdata, mpi, scenBase = scenBase, referenceYear = referenceYear)
  }
  return(mifdata)
}
