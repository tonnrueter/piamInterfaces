#' Checks for a run if the variables sum up as expected and logs spotted gaps
#'
#' @md
#' @author Falk Benke
#' @param mifFile path to the mif file to apply summation checks to
#' @param outputDirectory path to directory to place generated files (default: output)
#' @importFrom dplyr group_by summarise ungroup left_join mutate arrange %>% filter select desc
#' @importFrom quitte read.quitte
#' @importFrom rlang sym syms
#' @importFrom utils write.table
#'
#' @export
checkSummations <- function(mifFile, outputDirectory = "output") {
  if (!is.null(outputDirectory) && !dir.exists(outputDirectory)) {
    dir.create(outputDirectory, recursive = TRUE)
  }

  .calculateCheckSum <- function(name, x) {
    tmp <- x %>%
      group_by(!!!syms(c("model", "scenario", "region", "period"))) %>%
      summarise(checkSum = sum(!!sym("value") * !!sym("factor")), .groups = "drop") %>%
      ungroup()
    tmp$variable <- name
    return(tmp)
  }

  # FIXME: decide if this should become a parameter
  summationGroups <- read.csv2(system.file("summations", "summation_groups_ar6.csv",
    package = "piamInterfaces"
  ), sep = ";", stringsAsFactors = FALSE)

  data <- read.quitte(mifFile) %>%
    filter(!!sym("variable") %in% unique(c(summationGroups$child, summationGroups$parent))) %>%
    left_join(summationGroups, by = c("variable" = "child"))

  checkVariables <- list()

  for (i in unique(summationGroups$parent)) {
    checkVariables[[i]] <- summationGroups[which(summationGroups[, "parent"] == i), "child"]
  }

  names(checkVariables) <- gsub(" [1-9]$", "", names(checkVariables))

  tmp <- NULL
  for (i in names(checkVariables)) {
    tmp <- rbind(tmp, .calculateCheckSum(
      i,
      filter(data, !!sym("parent") == i, !!sym("variable") %in% checkVariables[[i]])
    ))
  }

  tmp <- left_join(tmp, data, c("scenario", "region", "variable", "period", "model")) %>%
    mutate(
      diff = abs(!!sym("checkSum") - !!sym("value")),
      reldiff = 100 * abs((!!sym("checkSum") - !!sym("value")) / !!sym("value"))
    ) %>%
    select(-c("factor", "parent"))

  fileSmall <- filter(tmp, !!sym("reldiff") < 1, !!sym("diff") < 0.001)
  fileLarge <- filter(tmp, !!sym("reldiff") >= 1, !!sym("diff") >= 0.001)

  write.table(
    arrange(fileLarge, desc(!!sym("reldiff"))), sep = ";",
    file = paste0(outputDirectory, "/checkSummations.csv"),
    quote = FALSE, row.names = FALSE
  )
  write.table(
    arrange(fileSmall, desc(!!sym("reldiff"))), sep = ";",
    file = paste0(outputDirectory, "/checkSummations_tiny.csv"),
    quote = FALSE, row.names = FALSE
  )
}
