#' niceround returns human-readable string for big and small numbers
#' Show everything before the dot and at least 'digits' significant digits
#' @param x some number
#' @param digits number of significant digits that are definitely kept
#' @export
niceround <- function(x, digits = 3) {
  x <- as.numeric(x)
  digits <- pmax(digits, ceiling(log10(abs(x))))
  return(format(signif(x, digits), scientific = FALSE))
}
