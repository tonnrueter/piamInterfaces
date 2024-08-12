test_that("renameOldVariables() works", {
  oldvar <- qeAR6
  newtemperature <- "MAGICC7 AR6|Surface Temperature (GSAT)|50.0th Percentile"
  expect_message(newvar <- renameOldVariables(oldvar, variables = newtemperature),
                 "Automatically adjusted variables based")
  expect_true(newtemperature %in% levels(newvar$variable))
  expect_false("Temperature|Globa Mean" %in% levels(newvar$variable))
  expect_identical(nrow(oldvar), nrow(newvar))

  oldname <- "Resources|Land Cover|Cropland|Bioenergy crops|+|Short rotation grasses"
  newname <- "Resources|Land Cover|Cropland|Croparea|Bioenergy crops|Short rotation grasses"
  levels(oldvar$variable)[[1]] <- oldname
  expect_message(newvar <- renameOldVariables(oldvar, variables = newname),
                 "Automatically adjusted variables based")
  expect_true(newname %in% levels(newvar$variable))
  expect_false(oldname %in% levels(newvar$variable))
  expect_identical(nrow(oldvar), nrow(newvar))

  levels(oldvar$variable)[[1]] <- "Energy Investments|Elec|Wind"
  expect_message(newvar <- renameOldVariables(oldvar, variables = "Energy Investments|Electricity|Wind"),
                 "Automatically adjusted variables based")
  expect_identical(levels(newvar$variable)[[1]], "Energy Investments|Electricity|Wind")
  expect_identical(nrow(oldvar), nrow(newvar))
  expect_identical(droplevels(select(quitteSort(oldvar), -"variable")),
                   droplevels(select(quitteSort(newvar), -"variable")))
})

test_that("no renamed_piam_variable used in mapping", {
  for (n in names(mappingNames())) {
    mapvars <- getMapping(n)$piam_variable
    oldvars <- getExpandRenamedVariables(mapvars)
    if (nrow(oldvars) > 0) {
      warning("In mapping_", n, ".csv, those variables are stated as renamed in inst/renamed_piam_variables.csv and",
              " should use the new name.\nIf the old variable name is passed in the data, it will work anyway:\n",
              paste(oldvars$old_name, "->", oldvars$piam_variable, collapse = "\n"))
    }
    expect_true(nrow(oldvars) == 0)
  }
})
