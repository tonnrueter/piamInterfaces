#' add variables that are missing based on a list of formulas
#'
#' @md
#' @author Oliver Richters
#' @param mifdata quitte object or filename of mif file
#' @param variables the list of requested variables
#' @param logFile filename of file for logging
#' @importFrom dplyr filter
#' @importFrom piamutils deletePnus
#' @importFrom rlang .datan
#' @importFrom tibble as_tibble
#' @return quitte object with adapted mif data
#' @export

renameOldVariables <- function(mifdata, variables, logFile = NULL) {
  mifdata <- as.quitte(mifdata)

  toadd <- unique(setdiff(variables, levels(mifdata$variable)))
  csvdata <- getExpandRenamedVariables(levels(mifdata$variable)) %>%
    filter(.data$piam_variable %in% toadd)
  old2new <- csvdata$piam_variable
  names(old2new) <- csvdata$old_name
  for (v in names(old2new)) {
    mifdata <- mifdata %>%
      mutate(variable = factor(ifelse(deletePlus(.data$variable) %in% v, old2new[[v]], deletePlus(as.character(.data$variable)))))
  }

  if (length(old2new) > 0 && ! isFALSE(logFile)) {
    logtext <- c("Automatically adjusted variables based on renamed_piam_variables.csv:\n",
                 paste("-", names(old2new), "->", old2new, collapse = "\n"))
    if (is.null(logFile)) message(logtext) else  write(logtext, file = logFile, append = TRUE)
  }

  return(mifdata)
}

getExpandRenamedVariables <- function(variables) {
  csvdata <- system.file("renamed_piam_variables.csv", package = "piamInterfaces") %>%
    read.csv2(comment.char = "#", strip.white = TRUE) %>%
    as_tibble() %>%
    mutate(piam_variable = deletePlus(.data$piam_variable),
           old_name = deletePlus(.data$old_name))
  variables <- deletePlus(variables)

  csvdataNew <- NULL
  for (i in seq(nrow(csvdata))) {
    # prepare strings for grepping by adding escape characters and "."
    if (all(grepl("\\*$", c(csvdata$piam_variable[i], csvdata$old_name[i])))) {
      matchOld <- sub(".$", "", csvdata$old_name[i])
      postfix <- gsub(matchOld, "", grep(matchOld, variables, fixed = TRUE, value = TRUE), fixed = TRUE)
    } else {
      postfix <- ""
    }
    csvdataNew <- rbind(
      csvdataNew,
      data.frame(piam_variable = paste0(gsub("\\*$", "", csvdata$piam_variable[i]), postfix),
                 old_name = paste0(gsub("\\*$", "", csvdata$old_name[i]), postfix))
    )
  }
  return(filter(csvdataNew, .data$old_name %in% variables))
}
