#' add missing variable values if the value can be obtained from two other reported results.
#'
#' @md
#' @author Renato Rodrigues
#' @param mifFile path to mif file or a quitte object
#' @param summationsFile in inst/summations folder that describes the required summation groups,
#'        or path to summations file
#' @importFrom dplyr select mutate filter count pull group_by summarize relocate
#' @importFrom quitte as.quitte
#'
#' @export
#'
fillSummationPairs <- function(mifFile, summationsFile) {

  data <- quitte::as.quitte(mifFile, na.rm = TRUE)

  summationGroups <- getSummations(summationsFile) %>%
    mutate(parentVar = gsub(" [1-9]$", "", !!sym("parent")))

  # summation groups with two child members, in which the parent and one child have reported values
  pairSummationsGroups <- summationGroups %>%
    filter(!!sym("parent") %in%
             (summationGroups %>% count(!!sym("parent")) %>% filter(!!sym("n") == 2) %>% pull(!!sym("parent"))),
           !!sym("parent") %in% unique(data$variable)) %>%
    mutate("childData" = ifelse(!!sym("child") %in% unique(data$variable), 1, 0)) %>%
    group_by(!!sym("parent")) %>%
    filter(sum(!!sym("childData")) == 1)

  # calculated variables for the missing child element of the pair summations
  extraVars <- do.call("rbind", lapply(unique(pairSummationsGroups$parent), function(currParent) {
    childWithValue <- pairSummationsGroups %>%
      filter(!!sym("parent") == currParent, !!sym("childData") == 1) %>%
      pull(!!sym("child"))
    childWithoutValue <- pairSummationsGroups %>%
      filter(!!sym("parent") == currParent, !!sym("childData") == 0) %>%
      pull(!!sym("child"))
    tmp <- data %>%
      filter(!!sym("variable") %in% c(currParent, childWithValue)) %>%
      mutate("value" = ifelse(!!sym("variable") == childWithValue, - !!sym("value"), !!sym("value"))) %>%
      group_by(!!!syms(c("model", "scenario", "region", "unit", "period"))) %>%
      summarize(value = sum(.data$value), .groups = "drop") %>%
      mutate("variable" = childWithoutValue) %>%
      group_by(!!!syms(c("model", "scenario", "region", "unit", "period"))) %>%
      relocate(!!!syms(c("model", "scenario", "region", "variable", "unit", "period", "value")))
    if (all(tmp %>% pull(!!sym("value")) == 0)) {
      return()
    } else {
      message(paste0("adding variable '", childWithoutValue, "' that can be calculated from ",
                     currParent, " - ", childWithValue))
      return(tmp)
    }
  })
  )

  # merge the missing childs data with the original data
  data <- rbind(data, extraVars)

  return(data)
}
