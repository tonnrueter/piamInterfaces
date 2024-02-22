test_that("readMifs works", {
  filename <- file.path(tempdir(), "readMifs", "testfile.mif")
  dir.create(dirname(filename), showWarnings = FALSE)
  mifstring <- c(
    "Model;Scenario;Region;Variable;Unit;2005;2010",
    "REMIND;NPi;GLO;FE;EJ/yr;1;2",
    "REMIND;NPi;GLO;PE;EJ/yr;2;3"
  )
  writeLines(mifstring, filename)
  d <- as.quitte(filename)
  expect_identical(d, readMifs(d))
  expect_identical(d, readMifs(filename))
  expect_identical(d, readMifs(dirname(filename)))
  expect_identical(rbind(d, d), readMifs(filename, dirname(filename)))
})
