test_that("renameOldVariables() works", {
  oldvar <- qeAR6
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
    oldvars <- system.file("renamed_piam_variables.csv", package = "piamInterfaces") %>%
      read.csv2(comment.char = "#", strip.white = TRUE) %>%
      as_tibble() %>%
      filter(.data$old_name %in% getMapping(n)$piam_variable)
    if (nrow(oldvars) > 0) {
      warning("In mapping_", n, ".csv, those variables are stated as renamed in inst/renamed_piam_variables.csv and",
              " should use the new name.\nIf the old variable name is passed in the data, it will work anyway:\n",
              paste(oldvars$old_name, "->", oldvars$piam_variable, collapse = "\n"))
    }
    expect_true(nrow(oldvars) == 0)
  }
})
