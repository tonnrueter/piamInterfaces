minimalLength <- list("AR6" = 1900, "NAVIGATE" = 1900)
for (template in names(templateNames())) {
  test_that(paste("basic checks on template", template), {
    expect_silent(templateData <- getTemplate(template))
    expect_true(all(c("variable", "unit", "piam_variable", "piam_unit", "piam_factor") %in% names(templateData)))
    expect_true(class(templateData) == "data.frame")
    expect_true(length(templateData$variable) > max(0, unlist(minimalLength[template])))
    expect_true(sum(is.na(templateData$variable)) == 0)
    conflictsigns <- grep("===|<<<|>>>", templateData[, 1], value = TRUE)
    if (length(conflictsigns) > 0) {
      warning("Lines that look like merge conflicts:\n", paste(conflictsigns, collapse = "\n"))
    }
    expect_true(length(conflictsigns) == 0)
  })
}
