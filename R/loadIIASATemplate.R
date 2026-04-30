#' Loads IIASA template (xlsx or yaml)
#'
#' @md
#' @author Oliver Richters
#' @param iiasatemplate filename of xlsx or yaml file provided by IIASA
#' @importFrom dplyr filter rename rename_with
#' @importFrom readxl read_excel
#' @importFrom stringr str_sub
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_wider
#' @importFrom yaml read_yaml
#' @examples
#' \dontrun{
#' # Simple use. Generates submission file in output folder:
#' loadIIASATemplate(
#'   iiasatemplate <- "template.xlsx"
#' )
#' }
#' @export
loadIIASATemplate <- function(iiasatemplate) {
  if (grepl("^https:\\/\\/files\\.ece\\.iiasa\\.ac\\.at\\/.*\\.xlsx$", iiasatemplate)) {
    tmpfile <- file.path(tempdir(), basename(iiasatemplate))
    utils::download.file(iiasatemplate, tmpfile, mode = "wb")
    iiasatemplate <- tmpfile
  }
  if (! file.exists(iiasatemplate)) {
    stop("# iiasatemplate ", iiasatemplate, " does not exist, unable to load it.")
  }
  if (str_sub(iiasatemplate, -5, -1) == ".xlsx") {
    for (i in seq(20)) {
      template <- rename_with(read_excel(iiasatemplate, sheet = i, guess_max = 21474836), tolower)
      if ("variable" %in% names(template)) {
        break
      }
    }
  } else if (str_sub(iiasatemplate, -5, -1) == ".yaml") {
    template <- unlist(read_yaml(iiasatemplate), recursive = FALSE)
    template <- rename(rename_with(unnest_wider(enframe(template), "value"), tolower), "variable" = "name")
  } else {
    stop("iiasatemplate ", iiasatemplate, " is neither xlsx nor yaml, so I don't understand it.")
  }
  if (length(template$variable) == 0) {
    stop("No 'variable' found in iiasatemplate ", iiasatemplate)
  }
  return(template)
}
