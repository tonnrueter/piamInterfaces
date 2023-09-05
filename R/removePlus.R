#' Remove |+|, |++| etc. from variable names
#'
#' @md
#' @author Oliver Richters
#' @param x vector with variable names
#' @return varname without any plusses human-readable summary to the user
#' @examples
#' #' removePlus(c("FE|+|CDR", "FE|CDR|DACCS"))
#' @export
removePlus <- function(x) {
  return(gsub("\\|\\++\\|", "|", x))
}
