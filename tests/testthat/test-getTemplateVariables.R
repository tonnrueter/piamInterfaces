test_that("getVariableTemplates works", {
  allvars <- c("FE (EJ/yr)", "Emi|Transport (Mt CO2/yr)")
  templatestring <- c(
    "variable;unit;piam_variable;piam_unit;piam_factor",
    "Final Energy;testunit;FE;EJ/yr;1",
    "Emissions|Transport;testunit;Emi|Transport;Mt CO2/yr;1"
  )
  sourcestring <- c(
    ";source",
    ";R",
    ";T"
  )
  templatefile <- file.path(tempdir(), "template.csv")
  # check behavior with no 'source' column
  writeLines(templatestring, templatefile)
  expect_error(getTemplateVariables(templatefile, "R"))
  expect_equal(getTemplateVariables(templatefile, TRUE), allvars)
  # check behavior with 'source' column
  writeLines(paste0(templatestring, sourcestring), templatefile)
  expect_equal(getTemplateVariables(templatefile, "R"), "FE (EJ/yr)")
  expect_equal(getTemplateVariables(templatefile, "T"), "Emi|Transport (Mt CO2/yr)")
  expect_equal(getTemplateVariables(templatefile, "RT"), allvars)
  expect_equal(getTemplateVariables(templatefile, "MRT"), allvars)
  expect_equal(getTemplateVariables(templatefile, TRUE), allvars)
  expect_equal(length(getTemplateVariables(templatefile, "M")), 0)
  expect_identical(getTemplateVariables(templatefile, "R"), getREMINDTemplateVariables(templatefile))
  for (t in names(templateNames())) {
    expect_no_error(vars <- getTemplateVariables(t, TRUE))
    expect_false(all(duplicated(vars)))
    expect_false(any(is.na(vars)))
  }
})
