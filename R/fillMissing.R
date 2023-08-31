#' Recursively calculate additional variables based on given summations and add
#' them to the given mif file
#'
#' @md
#' @author Falk Benke, Renato Rodrigues
#' @param mifFile path to mif file
#' @param summationsFile in inst/summations folder that describes the required summation groups,
#'        or path to summations file
#' @param iteration keeps track of number of recursive calls, leave to default
#' @importFrom dplyr select mutate filter count
#' @importFrom magclass as.magpie mbind
#'
#' @export
fillMissing <- function(mifFile, summationsFile, iteration = 1) {
  data <- quitte::as.quitte(mifFile, na.rm = TRUE)

  if (length(levels(data$model)) > 1) {
    message("This function cannot handle more than one model in the data yet.")
    return()
  }

  summationGroups <- getSummations(summationsFile) %>%
    mutate(parentVar = gsub(" [1-9]$", "", !!sym("parent")))

  # variables missing from data that can be calculated from summationGroups
  missingParentVars <- setdiff(summationGroups$parentVar, unique(data$variable))

  # summations don't contain any variables not in the data
  if (length(missingParentVars) == 0) {
    return(mifFile)
  }

  out <- NULL

  for (var in missingParentVars) {
    summations <- filter(summationGroups, !!sym("parentVar") == var)
    # loop over all summations for a parent variable
    for (sumId in unique(summations$parent)) {
      summation <- filter(summations, !!sym("parent") == sumId)
      children <- unique(summation$child)
      # any children are sufficient for a suitable summation
      if (any(children %in% unique(data$variable))) {
        message(paste0(
          "summation for '", sumId, "' found:\n ",
          paste0(intersect(children, unique(data$variable)), collapse = " + ")
        ))

        tmp <- filter(data, !!sym("variable") %in% children) %>%
          group_by(!!!syms(c("model", "scenario", "region", "period", "unit"))) %>%
          summarise(!!sym("value") := sum(!!sym("value")), .groups = "drop") %>%
          ungroup() %>%
          mutate(variable = var, sum_id = sumId)

        out <- rbind(out, tmp)
      }
    }
  }

  # no new variables could be added based on the summations file
  if (is.null(out)) {
    return(mifFile)
  }

  # if there is more than one summation for the same variable, pick the best one
  .pickSummation <- function(df) {
    # tolerance: 1e-3
    df <- df %>% mutate(!!sym("value") := round(!!sym("value"), 3))

    # check if the alternative summations yield the same results
    differences <- df %>%
      group_by(!!!syms(c("model", "scenario", "region", "period", "unit"))) %>%
      count(!!sym("value"), name = "count") %>%
      ungroup() %>%
      filter(!!sym("count") == 1) %>%
      select(-"count") %>%
      arrange(!!sym("period")) %>%
      left_join(df, by = c("model", "scenario", "region", "period", "unit", "value"))

    # some values differ, we must pick the best calculation using some heuristics
    if (nrow(differences) > 0) {
      if (nrow(filter(differences, !!sym("region") == "EUR", !!sym("period") == 2020)) > 0) {
        # if EUR 2020 has different values, this is the point of reference
        d <- filter(differences, !!sym("region") == "EUR", !!sym("period") == 2020)
      } else {
        # otherwise, use first period and region combo in the data
        d <- differences %>%
          filter(!!sym("period") == min(!!sym("period")))
        d <- d %>%
          filter(!!sym("region") == unique(!!sym("region"))[1])
      }

      # return calculation with the largest sum
      return(as.character(d[d$value == max(d$value), "sum_id"]))
    } else {
      # any calculation works, as they yield the same results
      return(unique(df$sum_id)[1])
    }
  }

  # look at variables that could be calculated using two or more summations
  alternativeVars <- select(out, c("variable", "sum_id")) %>%
    distinct() %>%
    dplyr::count(!!sym("variable")) %>%
    filter(!!sym("n") > 1)


  # eliminate all but one of the alternative calculations
  for (var in unique(alternativeVars$variable)) {
    ref <- filter(out, !!sym("variable") == var)
    keepSumId <- .pickSummation(ref)
    message(paste0(
      "keep summation '", keepSumId, "' for '", var, "', removing ",
      paste0(setdiff(unique(ref$sum_id), keepSumId), collapse = " , ")
    ))
    out <- filter(out, !!sym("variable") != var | !!sym("sum_id") == keepSumId)
  }

  # merge newly calculated data with input mif and make sure that structure
  # remains unchanged
  newMifFile <- out %>%
    select(-"sum_id") %>%
    mutate(!!sym("variable") := paste0(!!sym("variable"), " (", !!sym("unit"), ")")) %>%
    select(c("scenario", "model", "region", "variable", "year" = "period", "value")) %>%
    as.magpie(spatial = "region", temporal = "year", data = "value", tidy = TRUE) %>%
    mbind(mifFile)

  newVars <- paste0(unique(out$variable), collapse = ", ")
  message(paste0("Newly added vars in iteration ", iteration, ": ", newVars))

  # recursion: try running fillMissing again with the newly calculated variables
  return(fillMissing(newMifFile, summationsFile, iteration = iteration + 1))
}
