test_that("getMappingVariables works", {
  allvars <- c("FE (EJ/yr)", "Emi|Transport (Mt CO2/yr)")
  mappingstring <- c(
    "variable;unit;piam_variable;piam_unit;piam_factor",
    "Final Energy;testunit;FE;EJ/yr;1",
    "Emissions|Transport;testunit;Emi|Transport;Mt CO2/yr;1"
  )
  sourcestring <- c(
    ";source",
    ";R",
    ";T"
  )
  mappingfile <- file.path(tempdir(), "mapping.csv")
  # check behavior with no 'source' column
  writeLines(mappingstring, mappingfile)
  expect_error(getMappingVariables(mappingfile, "R"))
  expect_equal(getMappingVariables(mappingfile, TRUE), allvars)
  # check behavior with 'source' column
  writeLines(paste0(mappingstring, sourcestring), mappingfile)
  expect_equal(getMappingVariables(mappingfile, "R"), "FE (EJ/yr)")
  expect_equal(getMappingVariables(mappingfile, "T"), "Emi|Transport (Mt CO2/yr)")
  expect_equal(getMappingVariables(mappingfile, "RT"), allvars)
  expect_equal(getMappingVariables(mappingfile, "MRT"), allvars)
  expect_equal(getMappingVariables(mappingfile, TRUE), allvars)
  expect_equal(length(getMappingVariables(mappingfile, "M")), 0)
  expect_identical(getMappingVariables(mappingfile, "R"), getREMINDTemplateVariables(mappingfile))
  for (t in names(mappingNames())) {
    expect_no_error(vars <- getMappingVariables(t, TRUE))
    expect_false(all(duplicated(vars)))
    expect_false(any(is.na(vars)))
  }
})
