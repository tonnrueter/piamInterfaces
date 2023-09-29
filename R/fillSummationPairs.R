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
fillSummationPairs <- function(mifFile, summationsFile){
  
  data <- quitte::as.quitte(mifFile, na.rm = TRUE)

  summationGroups <- getSummations(summationsFile) %>%
    mutate(parentVar = gsub(" [1-9]$", "", !!sym("parent")))

  # summation groups with two child members, in which the parent and one child have reported values
  pairSummationsGroups <- summationGroups %>% 
    filter(parent %in% (summationGroups %>% count(parent) %>% filter(n == 2) %>% pull(parent)),
           parent %in% unique(data$variable)) %>%
    mutate(childData = ifelse(child %in% unique(data$variable), 1, 0)) %>% 
    group_by(parent) %>%
    filter(sum(childData) == 1)
  
  # calculated variables for the missing child element of the pair summations 
  extraVars <- do.call("rbind",lapply(unique(pairSummationsGroups$parent), function(par){
    childWithValue <- pairSummationsGroups %>% filter(parent == par, childData == 1) %>% pull(child)
    childWithoutValue <- pairSummationsGroups %>% filter(parent == par, childData == 0) %>% pull(child)
    message(paste0("adding variable '", childWithoutValue, "' that can be calculated from ", par , " - ", childWithValue))
    tmp <- data %>%
      filter(variable %in% c(par,childWithValue)) %>%
      mutate(value = ifelse(variable == childWithValue, - value, value)) %>%
      group_by(model,scenario,region,unit,period) %>%
      summarize(value = sum(value), .groups = 'drop') %>%
      mutate(variable = childWithoutValue) %>%
      relocate(model,scenario,region,variable,unit,period,value)
    return(tmp)
  })
  )
  
  # merge the missing childs data with the original data
  data <- rbind(data,extraVars)
  
  return(data)
}

