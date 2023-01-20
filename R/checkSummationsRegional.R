#' Checks for a run if the regions for selected variables sum up as expected
#'
#' @md
#' @author Falk Benke
#' @param mifFile path to the mif file to apply summation checks to, or quitte object
#' @param parentRegion region to sum up to
#' @param childRegions regions that should sum up to `parentRegion`
#' @param variables list of variables to check
#' @importFrom dplyr group_by summarise ungroup left_join mutate %>% filter select
#' @importFrom rlang sym syms
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
checkSummationsRegional <- function(mifFile, parentRegion, childRegions, variables) {

  data <- quitte::as.quitte(mifFile, na.rm = TRUE) %>%
    filter(!!sym("variable") %in% variables)

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
