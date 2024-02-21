test_that("basic checks on main templates", {
  minimalLength <- list("AR6" = 1900, "NAVIGATE" = 1900)
  for (template in names(templateNames())) {
    expect_silent(templateData <- getTemplate(template))
    expect_true(all(c("variable", "unit", "piam_variable", "piam_unit", "piam_factor") %in% names(templateData)))
    expect_true(class(templateData) == "data.frame")
    expect_true(length(templateData$variable) > max(0, unlist(minimalLength[template])))
  }
})
