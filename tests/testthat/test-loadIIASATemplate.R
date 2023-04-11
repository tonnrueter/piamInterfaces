test_that("loadIIASATemplate works", {

  templatedata <- tibble::as_tibble(data.frame(
    "variable" = c("Price|Energy", "Price|Energy|Index"),
    "unit" = c("US$2010/GJ", "Index (2005 = 1)")
  ))
  yamlstring <- c(
    "- Price|Energy:",
    "    unit: US$2010/GJ",
    "- Price|Energy|Index:",
    "    unit: Index (2005 = 1)"
  )

  # write iiasatemplate.xlsx
  iiasaxlsx <- file.path(tempdir(), "iiasatemplate.xlsx")
  writexl::write_xlsx(x = templatedata, path = iiasaxlsx)
  expect_identical(templatedata, loadIIASATemplate(iiasaxlsx))

  # write iiasatemplate.yaml
  iiasayaml <- file.path(tempdir(), "iiasatemplate.yaml")
  writeLines(yamlstring, iiasayaml)
  expect_identical(templatedata, loadIIASATemplate(iiasayaml))
})
