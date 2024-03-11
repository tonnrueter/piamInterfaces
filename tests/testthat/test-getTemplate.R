minimalLength <- list("AR6" = 1900, "NAVIGATE" = 1900)
for (template in names(templateNames())) {
  test_that(paste("basic checks on template", template), {
    expect_silent(templateData <- getTemplate(template))
    expect_true(all(c("variable", "unit", "piam_variable", "piam_unit", "piam_factor") %in% names(templateData)))
    expect_true(class(templateData) == "data.frame")
    expect_true(length(templateData$variable) > max(0, unlist(minimalLength[template])))
    # look for empty cells in variable column
    expect_true(sum(is.na(templateData$variable)) == 0)

    # look for merge conflicts
    conflictsigns <- grep("===|<<<|>>>", templateData[, 1], value = TRUE)
    if (length(conflictsigns) > 0) {
      warning("Lines that look like merge conflicts:\n", paste(conflictsigns, collapse = "\n"))
    }
    expect_true(length(conflictsigns) == 0, label = paste0(template, " has no merge conflicts"))

    # check for inconsistent variable + unit combinations
    nonempty <- dplyr::filter(templateData, ! is.na(.data$piam_variable), ! .data$piam_variable == "TODO")
    allVarUnit <- paste0(nonempty$piam_variable, " (", nonempty$piam_unit, ")")
    unclearVar <- nonempty$piam_variable[duplicated(nonempty$piam_variable) & ! duplicated(allVarUnit)]
    unclearVarUnit <- sort(unique(allVarUnit[nonempty$piam_variable %in% unclearVar]))
    if (length(unclearVarUnit)) {
      warning("These variables have inconsistent units:\n",
              paste(unclearVarUnit, collapse = "\n"))
    }
    expect_true(length(unclearVarUnit) == 0, label = paste("variables and units are consistent for", template))

    # checks only if source is supplied
    if ("source" %in% colnames(templateData)) {
      # check for empty piam_variable with source
      sourceWithoutVar <- templateData %>%
        filter(! is.na(.data$source), is.na(.data$piam_variable)) %>%
        pull("variable")
      if (length(sourceWithoutVar) > 0) {
        warning("These variables in template ", template, " have a source, but nothing specified in piam_variable:\n",
                paste(sourceWithoutVar, collapse = "\n"))
      }
      expect_true(length(sourceWithoutVar) == 0)

      # check for piam_variable without source if source is supplied
      varWithoutSource <- templateData %>%
        filter(! is.na(.data$piam_variable), ! .data$piam_variable %in% "TODO", is.na(.data$source)) %>%
        pull("piam_variable") %>%
        unique()
      if (length(varWithoutSource) > 0) {
        warning("These piam_variable in template ", template, " have no source:\n",
                paste(varWithoutSource, collapse = "\n"))
      }
      expect_true(length(varWithoutSource) == 0)
    } # end source checks
  })
}
