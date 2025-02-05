#' writeMapping
#'
#' write data.frame obtained from getMapping() to mapping file.
#'
#' @md
#' @author Oliver Richters
#' @param mapping mapping data.frame
#' @param filename filename.
#' @importFrom utils write.table
#' @export
writeMapping <- function(mapping, filename) {
  write.table(mapping, filename, na = "",  dec = ".", sep = ";", row.names = FALSE, quote = FALSE)
}
