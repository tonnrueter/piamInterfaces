test_that("check that main templates are found", {
  expect_true(all(c("AR6", "NAVIGATE", "SHAPE") %in% names(templateNames())))
  expect_true(grepl("mapping_template_AR6.csv$", templateNames("AR6")))
  expect_true(grepl("mapping_template_NAVIGATE.csv$", templateNames("NAVIGATE")))
  expect_true(grepl("mapping_template_SHAPE.csv$", templateNames("SHAPE")))
  expect_true(file.exists(templateNames("AR6")))
  expect_true(file.exists(templateNames("NAVIGATE")))
  expect_true(file.exists(templateNames("SHAPE")))
})
