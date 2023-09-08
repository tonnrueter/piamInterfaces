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
  new <- gsub("\\|\\++\\|", "|", x)
  if (any(grepl("\\|\\++\\|", new))) {
    return(removePlus(new))
  } else {
    return(new)
  }
}
