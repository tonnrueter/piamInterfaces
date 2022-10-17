test_that("basic checks on main templates", {
  minimalLength <- list("AR6" = 1900, "NAVIGATE" = 1900)
  for (template in names(templateNames())) {
    expect_silent(templateData <- getTemplate(template))
    expect_true(all(c("idx", "Variable", "Unit", "r30m44", "r30m44_unit", "r30m44_factor",
                      "internal_comment", "Comment", "Definition") %in% names(templateData)))
    expect_true(class(templateData) == "data.frame")
    expect_true(length(templateData$Variable) > max(0, unlist(minimalLength[template])))
  }
})
