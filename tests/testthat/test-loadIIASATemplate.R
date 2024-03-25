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

test_that("All variables in MAGICC7 template are mapped to something from REMIND via AR6 mapping", {

  # check whether all variables in MAGICC template have proper piam_variable
  t <- getMapping("AR6")
  magiccfile <- file.path(system.file("iiasaTemplates", package = "piamInterfaces"),
                          "climate_assessment_variables.yaml")
  magicc <- loadIIASATemplate(magiccfile)
  expect_true("variable" %in% colnames(magicc))
  expect_true("unit" %in% colnames(magicc))
  magiccvars <- paste0(magicc$variable, " (", magicc$unit, ")")
  tvars <- paste0(t$variable, " (", t$unit, ")")[! is.na(t$piam_variable)]
  missingvars <- setdiff(magiccvars, tvars)
  if (length(missingvars) > 0) {
    warning("MAGICC7 variables with no piam_variable in AR6 template: ",
            paste0(missingvars, collapse = ", "))
  }
  expect_true(length(missingvars) == 0)
})
