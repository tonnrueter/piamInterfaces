data <- magclass::new.magpie(cells_and_regions = "GLO", years = c(2030, 2050), fill = c(2, 4, 1, 2, 1, 2),
                      names = c("Final Energy|Industry (EJ)", "Final Energy|Industry|Electricity (EJ)",
                                "Final Energy|Industry|Liquids (EJ)"))
magclass::getSets(data)[3] <- "variable"
data <- magclass::add_dimension(data, dim = 3.1, add = "model", nm = "REMIND")
data <- magclass::add_dimension(data, dim = 3.1, add = "scenario", nm = "default")
magclass::write.report(data, file = file.path(tempdir(), "test.mif"), ndigit = 0)

dataerror <- magclass::new.magpie(cells_and_regions = c("CAZ", "World"), years = c(2030, 2050),
                           fill = c(3, 5, 1, 2, 1, 2, 3, 5, 1, 2, 1, 2),
                           names = c("Final Energy|Industry (EJ)", "Final Energy|Industry|Electricity (EJ)",
                                     "Final Energy|Industry|Liquids (EJ)"))
magclass::getSets(dataerror)[3] <- "variable"
dataerror <- magclass::add_dimension(dataerror, dim = 3.1, add = "model", nm = "REMIND")
dataerror <- magclass::add_dimension(dataerror, dim = 3.1, add = "scenario", nm = "default")
magclass::write.report(dataerror, file = file.path(tempdir(), "testerror.mif"), ndigit = 0)


for (summationFile in names(summationsNames())) {
  test_that(paste("test summationFile without errors using", summationFile), {
    if (summationFile == "AR6") {
      expect_message(tmp <- checkSummations(data, logFile = NULL,
                             template = summationFile, summationsFile = summationFile, outputDirectory = tempdir(),
                             dataDumpFile = "checkSummations.csv"),
                     "All summation checks were fine")
    } else {
      expect_message(tmp <- checkSummations(mifFile = file.path(tempdir(), "test.mif"), logFile = NULL,
                             template = summationFile, summationsFile = summationFile, outputDirectory = tempdir(),
                             dataDumpFile = "checkSummations.csv"),
                     "All summation checks were fine")
    }
    expect_true(all(tmp$diff == 0))
  })
  test_that(paste("test summationFile with errors using ", summationFile), {
    if (summationFile == "AR6") {
      expect_message(tmp <- checkSummations(mifFile = dataerror, logFile = NULL,
                             template = summationFile, summationsFile = summationFile, outputDirectory = tempdir(),
                             dataDumpFile = "checkSummations.csv"),
                     "Final Energy")
      capture.output(expect_message(tmp <- checkSummations(mifFile = file.path(tempdir(), "testerror.mif"),
                             logFile = "log.txt", template = summationFile, summationsFile = summationFile,
                             outputDirectory = tempdir(), dataDumpFile = "checkSummations.csv", generatePlots = TRUE),
                     "1 equations are not satisfied"))
      expect_true(file.exists(file.path(tempdir(), "checkSummations_REMIND.pdf")))
      expect_true(file.info(file.path(tempdir(), "checkSummations_REMIND.pdf"))$size > 0)
    } else {
      expect_message(tmp <- checkSummations(mifFile = file.path(tempdir(), "testerror.mif"), logFile = NULL,
                             template = summationFile, summationsFile = summationFile, outputDirectory = tempdir(),
                             dataDumpFile = "checkSummations.csv"),
                     "Final Energy")
    }
    expect_false(all(tmp$diff == 0))
    expect_true(all(file.exists(file.path(tempdir(), c("log.txt", "checkSummations.csv")))))
  })
}
unlink(file.path(tempdir(), c("test.mif", "testerror.mif", "log.txt", "checkSummations.csv")))
