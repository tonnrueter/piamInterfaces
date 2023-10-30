#' niceround returns human-readable string for big and small numbers
#' Show everything before the dot and at least 'digits' significant digits
#' For a data.frame, convert all numeric columns
#' @param x either a vector or list of numbers, or a dateframe
#' @param digits number of significant digits that are definitely kept
#' @export
niceround <- function(x, digits = 3) {
  if ("data.frame" %in% class(x)) {
    for (n in colnames(x)[lapply(x, class) == "numeric"]) {
      x[n] <- niceround(x[[n]], digits = digits)
    }
    return(x)
  } else {
    x <- as.numeric(x)
    digits <- pmax(digits, ceiling(log10(abs(x))))
    # this construction is needed to make sure every value is rounded individually
    return(vapply(seq_along(x), function(i) format(signif(x[i], digits[i]), scientific = FALSE), character(1)))
  }
}
