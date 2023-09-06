#' niceround returns human-readable string for big and small numbers
#' If abs(x) > 1, show everything before the dot plus 2 digits after. For abs(x) < 1, show 3 digits
#' @param x some number
#' @export
niceround <- function(x) {
  digits <- pmax(3, 2 + ceiling(log10(abs(x))))
  return(format(signif(x, digits), scientific = FALSE))
}
