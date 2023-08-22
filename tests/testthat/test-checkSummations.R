varnames <- paste(c("Final Energy|Industry", "Final Energy|Industry|Electricity", "Final Energy|Industry|Liquids"),
                  "(EJ/yr)")

data <- magclass::new.magpie(cells_and_regions = "GLO", years = c(2030, 2050), fill = c(2, 4, 1, 2, 1, 2),
                             names = varnames)
magclass::getSets(data)[3] <- "variable"
data <- magclass::add_dimension(data, dim = 3.1, add = "model", nm = "REMIND")
data <- magclass::add_dimension(data, dim = 3.1, add = "scenario", nm = "default")
magclass::write.report(data, file = file.path(tempdir(), "test.mif"), ndigit = 0)

dataerror <- magclass::new.magpie(cells_and_regions = c("CAZ", "World"), years = c(2030, 2050),
                                  fill = c(3, 5, 1, 2, 1, 2, 3, 5, 1, 2, 1, 2),
                                  names = varnames)
magclass::getSets(dataerror)[3] <- "variable"
dataerror <- magclass::add_dimension(dataerror, dim = 3.1, add = "model", nm = "REMIND")
dataerror <- magclass::add_dimension(dataerror, dim = 3.1, add = "scenario", nm = "default")
magclass::write.report(dataerror, file = file.path(tempdir(), "testerror.mif"), ndigit = 0)


for (summationFile in names(summationsNames())) {
  test_that(paste("test summationFile without errors using", summationFile), {
    if (summationFile == "AR6") {
      expect_message(tmp <- checkSummations(data, logFile = NULL,
                                            template = summationFile, summationsFile = summationFile,
                                            outputDirectory = tempdir(),
                                            dataDumpFile = "checkSummations1.csv"),
                     "All summation checks were fine")
      expect_true(file.exists(file.path(tempdir(), "checkSummations1.csv")))
    } else {
      expect_message(tmp <- checkSummations(mifFile = file.path(tempdir(), "test.mif"), logFile = NULL,
                                            template = summationFile, summationsFile = summationFile,
                                            outputDirectory = tempdir(),
                                            dataDumpFile = "checkSummations2.csv"),
                     "All summation checks were fine")
      expect_true(file.exists(file.path(tempdir(), "checkSummations2.csv")))
    }
    tmp <- droplevels(filter(tmp, !is.na(tmp$value)))
    expect_true(all(tmp$diff == 0))
    expect_true(length(tmp$diff) > 0)
    expect_true(any(grepl("^Final Energy\\|Industry( [0-9]+)?$", unique(tmp$variable))))
  })
  test_that(paste("test summationFile with errors using", summationFile), {
    if (summationFile == "AR6") {
      expect_message(tmp <- checkSummations(mifFile = dataerror, logFile = NULL,
                                            template = summationFile, summationsFile = summationFile,
                                            outputDirectory = tempdir(),
                                            dataDumpFile = "checkSummations3.csv"),
                     "Final Energy|Industry", fixed = TRUE)
      expect_true(file.exists(file.path(tempdir(), "checkSummations3.csv")))
      capture.output(expect_message(tmp <- checkSummations(mifFile = file.path(tempdir(), "testerror.mif"),
                                                           logFile = "log4.txt", template = summationFile,
                                                           summationsFile = summationFile,
                                                           outputDirectory = tempdir(),
                                                           dataDumpFile = "checkSummations4.csv", generatePlots = TRUE,
                                                           plotprefix = "TESTTHAT_"),
                                    "1 equations are not satisfied"))
      expect_true(file.exists(file.path(tempdir(), "TESTTHAT_checkSummations_REMIND.pdf")))
      expect_true(file.info(file.path(tempdir(), "TESTTHAT_checkSummations_REMIND.pdf"))$size > 0)
      expect_true(file.exists(file.path(tempdir(), "log4.txt")))
      expect_true(file.exists(file.path(tempdir(), "checkSummations4.csv")))
    } else {
      expect_message(tmp <- checkSummations(mifFile = file.path(tempdir(), "testerror.mif"), logFile = NULL,
                                            template = summationFile,
                                            summationsFile = summationFile, outputDirectory = tempdir(),
                                            dataDumpFile = "checkSummations3.csv"),
                     "Final Energy|Industry", fixed = TRUE)
      expect_true(file.exists(file.path(tempdir(), "checkSummations3.csv")))
    }
    expect_false(all(tmp$diff == 0))
  })
}
unlink(file.path(tempdir(), c("test.mif", "testerror.mif", "log.txt", "checkSummations.csv")))
