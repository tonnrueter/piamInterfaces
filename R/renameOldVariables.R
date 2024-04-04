#' add variables that are missing based on a list of formulas
#'
#' @md
#' @author Oliver Richters
#' @param mifdata quitte object or filename of mif file
#' @param variables the list of requested variables
#' @param logFile filename of file for logging
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @return quitte object with adapted mif data
#' @export

renameOldVariables <- function(mifdata, variables, logFile = NULL) {
  mifdata <- as.quitte(mifdata)

  toadd <- unique(setdiff(variables, levels(mifdata$variable)))
  csvdata <- system.file("renamed_piam_variables.csv", package = "piamInterfaces") %>%
    read.csv2(comment.char = "#", strip.white = TRUE) %>%
    as_tibble() %>%
    filter(.data$piam_variable %in% toadd, .data$old_name %in% levels(mifdata$variable))
  old2new <- csvdata$piam_variable
  names(old2new) <- csvdata$old_name
  # we need to loop because the second calculation might need data from the first
  for (v in names(old2new)) {
    mifdata <- mifdata %>%
      mutate(variable = factor(ifelse(.data$variable == v, old2new[[v]], as.character(.data$variable))))
  }

  if (length(old2new) > 0 && ! is.null(logFile) && ! isFALSE(logFile)) {
    logtext <- c("Automatically added variables based on renamed_piam_variables.csv:\n",
                 paste(names(old2new), " -> ", old2new, "\n", collapse = ", "))
    write(logtext, file = logFile, append = TRUE)
  }

  return(mifdata)
}
