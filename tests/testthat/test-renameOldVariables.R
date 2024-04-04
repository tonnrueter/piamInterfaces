test_that("renameOldVariables() works", {
  oldvar <- qeAR6
  levels(oldvar$variable)[[1]] <- "Energy Investments|Elec|Wind"
  newvar <- renameOldVariables(oldvar, variables = "Energy Investments|Electricity|Wind")
  expect_identical(levels(newvar$variable)[[1]], "Energy Investments|Electricity|Wind")
  expect_identical(nrow(oldvar), nrow(newvar))
  expect_identical(droplevels(select(quitteSort(oldvar), -"variable")),
                   droplevels(select(quitteSort(newvar), -"variable")))
})
