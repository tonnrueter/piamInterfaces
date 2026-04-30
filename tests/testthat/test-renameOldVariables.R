test_that("renamed_piam_variables wildcards match", {
  csvdata <- system.file("renamed_piam_variables.csv", package = "piamInterfaces") %>%
    read.csv2(comment.char = "#", strip.white = TRUE) %>%
    as_tibble()
  inconsistentAsterisk <- filter(csvdata) %>%
    filter(grepl("\\*", .data$piam_variable) | grepl("\\*", .data$old_name)) %>%
    filter(! grepl("^[^\\*]+\\*$", .data$piam_variable) | ! grepl("^[^\\*]+\\*$", .data$old_name))
  if (nrow(inconsistentAsterisk) > 0) {
    warning("In inst/renamed_piam_variables.csv, you can have either no * at all or an * at the end of both columns.\n",
            "These lines do not match this pattern:\n",
            paste(inconsistentAsterisk$old_name, "->", inconsistentAsterisk$piam_variable, collapse = "\n"))
  }
  expect_true(nrow(inconsistentAsterisk) == 0)
})

test_that("renamed_piam_variables has no half-empty lines", {
  onlyOne <- readRenames() %>%
    filter(.data$old_name %in% c("", NA) | .data$piam_variable %in% c("", NA))
  if (nrow(onlyOne) > 0) {
    warning("In inst/renamed_piam_variables.csv, these lines are partially empty:\n",
            paste0("- ", onlyOne$piam_variable, ";", onlyOne$old_name, collapse = "\n"))
  }
  expect_length(onlyOne$piam_variable, 0)
})

test_that("renamed_piam_variables has no duplicates", {
  oldNames <- pull(readRenames(), "old_name")
  duplicates <- oldNames[duplicated(oldNames)]
  if (length(duplicates) > 0) {
    warning("In inst/renamed_piam_variables.csv, these old_name variables are duplicates:\n",
            paste("-", duplicates, collapse = "\n"))
  }
  expect_length(duplicates, 0)

  nodiff <- readRenames() %>%
    filter(deletePlus(.data$old_name) == deletePlus(.data$piam_variable))
  if (nrow(nodiff) > 0) {
    warning("In inst/renamed_piam_variables.csv, these piam_variable and old_name are identical:\n",
            paste("-", nodiff$piam_variable, "<-", nodiff$old_name, collapse = "\n"))
  }
  expect_true(nrow(nodiff) == 0)

})

test_that("renameOldVariables() works", {
  oldvar <- qeAR6
  newtemperature <- "MAGICC7 AR6|Surface Temperature (GSAT)|50.0th Percentile"
  expect_message(newvar <- renameOldVariables(oldvar, variables = newtemperature),
                 "Automatically adjusted variables based")
  expect_true(newtemperature %in% newvar$variable)
  expect_false("Temperature|Global Mean" %in% newvar$variable)
  expect_identical(nrow(oldvar), nrow(newvar))

  oldname <- "Resources|Land Cover|Cropland|Bioenergy crops|+|Short rotation grasses"
  newname <- "Resources|Land Cover|Cropland|Croparea|Bioenergy crops|Short rotation grasses"
  levels(oldvar$variable)[[1]] <- oldname
  expect_message(newvar <- renameOldVariables(oldvar, variables = newname),
                 "Automatically adjusted variables based")
  expect_true(newname %in% newvar$variable)
  expect_false(deletePlus(oldname) %in% deletePlus(newvar$variable))
  expect_identical(nrow(oldvar), nrow(newvar))

  levels(oldvar$variable)[[1]] <- "Energy Investments|Elec|Wind"
  expect_message(newvar <- renameOldVariables(oldvar, variables = "Energy Investments|Electricity|Wind"),
                 "Automatically adjusted variables based")
  expect_identical(levels(newvar$variable)[[1]], "Energy Investments|Electricity|Wind")
  expect_identical(nrow(oldvar), nrow(newvar))
  expect_identical(droplevels(select(quitteSort(oldvar), -"variable")),
                   droplevels(select(quitteSort(newvar), -"variable")))
})

for (n in names(mappingNames())) {
  test_that(paste("no renamed_piam_variable used in", n), {
    mapvars <- getMapping(n)$piam_variable
    oldvars <- getExpandRenamedVariables(mapvars)
    if (nrow(oldvars) > 0) {
      warning("In mapping_", n, ".csv, those variables are stated as renamed in inst/renamed_piam_variables.csv and",
              " should use the new name.\nIf the old variable name is passed in the data, it will work anyway:\n",
              paste("-", oldvars$old_name, "->", oldvars$piam_variable, collapse = "\n"),
              "\nTry to run the following, see also tutorial: Rscript -e 'devtools::load_all(); renameOldInMappings()'")
    }
    expect_true(nrow(oldvars) == 0)
  })
}
