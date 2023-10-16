#' niceround returns human-readable string for big and small numbers
#' Show everything before the dot and at least 'digits' significant digits
#' @param x some number
#' @param digits number of significant digits that are definitely kept
#' @export
niceround <- function(x, digits = 3) {
  x <- as.numeric(x)
  digits <- pmax(digits, ceiling(log10(abs(x))))
  # this construction is needed to make sure every value is rounded individually
  return(vapply(seq_along(x), function(i) format(signif(x[i], digits[i]), scientific = FALSE), character(1)))
}
