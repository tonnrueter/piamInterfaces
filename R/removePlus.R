#' Remove |+|, |++| etc. from variable names
#'
#' @md
#' @author Oliver Richters
#' @param x vector with variable names
#' @importFrom piamutils deletePlus
#' @return variable names without any plus notation
#' @examples
#' #' removePlus(c("FE|+|CDR", "FE|CDR|DACCS"))
#' @export
removePlus <- function(x) {
  return(deletePlus(x))
}
