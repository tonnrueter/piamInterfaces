test_that("check that main mappings are found", {
  expect_true(all(c("AR6", "NAVIGATE", "ScenarioMIP") %in% names(mappingNames())))
  expect_true(grepl("mapping_AR6.csv$", mappingNames("AR6")))
  expect_true(grepl("mapping_NAVIGATE.csv$", mappingNames("NAVIGATE")))
  expect_true(grepl("mapping_ScenarioMIP.csv$", mappingNames("ScenarioMIP")))
  expect_true(file.exists(mappingNames("AR6")))
  expect_true(file.exists(mappingNames("NAVIGATE")))
  expect_true(file.exists(mappingNames("ScenarioMIP")))
  expect_equal(basename(list.files("../../inst/mappings", pattern = "mapping_.*\\.csv")),
               basename(unname(mappingNames())))
})
