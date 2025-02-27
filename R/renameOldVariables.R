#' add variables that are missing based on a list of formulas
#'
#' @md
#' @author Oliver Richters
#' @param mifdata quitte object or filename of mif file
#' @param variables the list of requested variables
#' @param logFile filename of file for logging
#' @importFrom dplyr filter distinct
#' @importFrom piamutils deletePlus
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @return quitte object with adapted mif data
#' @export

renameOldVariables <- function(mifdata, variables, logFile = NULL) {
  mifdata <- deletePlus(as.quitte(mifdata))

  toadd <- unique(setdiff(variables, levels(mifdata$variable)))
  csvdata <- getExpandRenamedVariables(levels(mifdata$variable)) %>%
    mutate(piam_variable = deletePlus(.data$piam_variable)) %>%
    filter(.data$piam_variable %in% toadd)
  old2new <- csvdata$piam_variable
  names(old2new) <- csvdata$old_name
  for (v in names(old2new)) {
    mifdata <- mifdata %>%
      mutate(variable = factor(ifelse(.data$variable %in% v, old2new[[v]], as.character(.data$variable))))
  }

  if (length(old2new) > 0 && ! isFALSE(logFile)) {
    logtext <- c("Automatically adjusted variables based on renamed_piam_variables.csv:\n",
                 paste("-", names(old2new), "->", old2new, collapse = "\n"))
    if (is.null(logFile)) message(logtext) else  write(logtext, file = logFile, append = TRUE)
  }

  return(mifdata)
}

getExpandRenamedVariables <- function(variables) {
  if (length(variables) == 0) return(NULL)
  csvdata <- readRenames()
  variables <- deletePlus(variables)

  duplicates <- csvdata$old_name[duplicated(csvdata$old_name)]
  if (length(duplicates) > 0) {
    warning("Duplicates in 'old_name' found: ", paste(duplicates, collapse = ", "))
  }

  csvdataNew <- NULL
  for (i in seq_len(nrow(csvdata))) {
    # if both end with *, replace by options taken from 'variables'
    if (all(grepl("\\*$", c(csvdata$piam_variable[i], csvdata$old_name[i])))) {
      matchOld <- sub("\\*$", "", csvdata$old_name[i])
      postfix <- gsub(matchOld, "", grep(matchOld, variables, fixed = TRUE, value = TRUE), fixed = TRUE)
      csvdataNew <- rbind(
        csvdataNew,
        data.frame(piam_variable = paste0(gsub("\\*$", "", csvdata$piam_variable[i]), postfix),
                   old_name = paste0(gsub("\\*$", "", csvdata$old_name[i]), postfix))
      )
    } else {
      csvdataNew <- rbind(csvdataNew, csvdata[i, ])
    }
  }
  # filter what is necessary and remove duplicated lines with distinct
  csvdataNew <- distinct(filter(csvdataNew, .data$old_name %in% variables))

  # check for remaining duplicates in old_name
  duplicates <- csvdata$old_name[duplicated(csvdata$old_name)]
  if (length(duplicates) > 0) {
    warning("Duplicates in 'old_name' found after expansion: ", paste(duplicates, collapse = ", "))
  }

  return(csvdataNew)
}

readRenames <- function() {
  system.file("renamed_piam_variables.csv", package = "piamInterfaces") %>%
    read.csv2(comment.char = "#", strip.white = TRUE) %>%
    as_tibble() %>%
    filter(! .data$old_name %in% c(NA, "") | ! .data$piam_variable %in% c(NA, "")) %>%
    mutate(old_name = deletePlus(.data$old_name))
}
