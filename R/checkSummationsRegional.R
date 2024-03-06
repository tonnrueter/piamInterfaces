#' Checks for a run if the regions for selected variables sum up as expected
#'
#' @md
#' @author Falk Benke
#' @param mifFile path to the mif file to apply summation checks to, or quitte object
#' @param parentRegion region to sum up to. Defaults to World or GLO
#' @param childRegions regions that should sum up to `parentRegion`. Default to all except parentRegion
#' @param variables list of variables to check. Defaults to all in mifFile
#' @importFrom dplyr group_by summarise ungroup left_join mutate %>% filter select
#' @importFrom rlang sym syms .data
#' @importFrom quitte as.quitte
#' @examples
#' \dontrun{
#'checkSummationsRegional(
#'   mifFile = "path/to/file",
#'   childRegions = c("R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF"),
#'   parentRegion = "World",
#'   variables = c("Final Energy|Industry", "Emissions|CO2|Energy|Demand|Industry")
#')
#'}
#' @export
checkSummationsRegional <- function(mifFile, parentRegion = NULL, childRegions = NULL, variables = NULL) {

  data <- droplevels(quitte::as.quitte(mifFile, na.rm = TRUE))
  if (! is.null(variables))  data <- filter(data, .data$variable %in% variables)
  if (is.null(parentRegion)) parentRegion <- intersect(c("World", "GLO"), levels(data$region))[[1]]
  if (is.null(childRegions)) childRegions <- setdiff(levels(data$region), parentRegion)

  sum <- filter(data, !!sym("region") %in% childRegions)  %>%
    group_by(!!!syms(c("model", "scenario", "variable", "period", "unit"))) %>%
    summarise(checkSum = sum(!!sym("value")), .groups = "drop") %>%
    ungroup()

  total <- filter(data, !!sym("region") == parentRegion) %>%
    rename("total" = "value") %>%
    select(-"region")

  tmp <- left_join(sum, total, by = c("model", "scenario", "variable", "period", "unit")) %>%
    mutate(
      diff = !!sym("checkSum") - !!sym("total"),
      reldiff = 100 * (!!sym("checkSum") - !!sym("total")) / !!sym("total")
    )

  return(tmp)
}
