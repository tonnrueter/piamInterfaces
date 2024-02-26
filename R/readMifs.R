#' Pass a character vector containing filenames and directories.
#' Returns data from all files and all '.mif' files in the directories.
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param ... path to mif files or directories with mif files of a REMIND run or quitte object
#' @examples
#' \dontrun{
#' # Simple use. Generates submission file in output folder:
#' readMifs(
#'   mifs = "/path/to/REMIMD/mifs",
#' )
#' }
#' @export
readMifs <- function(...) {
  miflist <- list(...)
  mifdata <- NULL
  for (mifs in miflist) {
    if (is.character(mifs)) {
      invalidElements <- intersect(mifs[!dir.exists(mifs)], mifs[!file.exists(mifs)])

      if (length(invalidElements) > 0) {
        stop(paste0("Invalid argument 'mifs'. Element(s) that are neither files nor paths: ",
                    paste0(invalidElements, collapse = ", ")))
      }

      for (m in mifs[dir.exists(mifs)]) {
        if (length(list.files(m, "*.mif")) == 0) {
          stop(paste0("No mif files found in folder ", m))
        }
      }

      flist <- unique(c(mifs[!dir.exists(mifs)], list.files(mifs[dir.exists(mifs)], "*.mif", full.names = TRUE)))
      message(paste0("# Reading in mifs ", paste0(flist, collapse = ", ")))
      mifdata <- rbind(mifdata, droplevels(as.quitte(flist), na.rm = TRUE))
    } else {
      mifdata <- droplevels(as.quitte(mifs, na.rm = TRUE))
    }
  }
  return(mifdata)
}
