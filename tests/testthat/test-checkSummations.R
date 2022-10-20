for (summationFile in names(summationsNames())) {
  test_that(paste("test summationFile without errors using ", summationFile), {
    vars <- NULL
    data <- magclass::new.magpie(cells_and_regions = "GLO", years = c(2010), fill = c(2, 1, 1),
                          names = c("Final Energy (EJ)", "Final Energy|Electricity (EJ)", "Final Energy|Liquids (EJ)"))
    magclass::getSets(data)[3] <- "variable"
    data <- magclass::add_dimension(data, dim = 3.1, add = "model", nm = "REMIND")
    data <- magclass::add_dimension(data, dim = 3.1, add = "scenario", nm = "default")
    magclass::write.report(data, file = file.path(tempdir(), "test.mif"), ndigit = 0)
    expect_message(tmp <- checkSummations(mifFile = file.path(tempdir(), "test.mif"), logFile = NULL,
                           template = "AR6", summationsFile = summationFile, outputDirectory = tempdir(),
                           dataDumpFile = "checkSummations.csv"),
                   "0 equations are not satisfied")
    expect_message(tmp <- checkSummations(mifFile = file.path(tempdir(), "test.mif"), logFile = "log.txt",
                           template = "AR6", summationsFile = summationFile, outputDirectory = tempdir(),
                           dataDumpFile = "checkSummations.csv"),
                   "Find log with human-readable information")
    expect_true(all(tmp$diff == 0))
    expect_true(all(file.exists(file.path(tempdir(), c("log.txt", "checkSummations.csv")))))
  })
  test_that(paste("test summationFile with errors using ", summationFile), {
    vars <- NULL
    data <- magclass::new.magpie(cells_and_regions = "GLO", years = c(2010), fill = c(1, 1, 1),
                          names = c("Final Energy (EJ)", "Final Energy|Electricity (EJ)", "Final Energy|Liquids (EJ)"))
    magclass::getSets(data)[3] <- "variable"
    data <- magclass::add_dimension(data, dim = 3.1, add = "model", nm = "REMIND")
    data <- magclass::add_dimension(data, dim = 3.1, add = "scenario", nm = "default")
    magclass::write.report(data, file = file.path(tempdir(), "test.mif"), ndigit = 0)
    expect_message(tmp <- checkSummations(mifFile = file.path(tempdir(), "test.mif"), logFile = NULL,
                           template = "AR6", summationsFile = summationFile, outputDirectory = tempdir(),
                           dataDumpFile = "checkSummations.csv"),
                   "Final Energy")
    expect_message(tmp <- checkSummations(mifFile = file.path(tempdir(), "test.mif"), logFile = "log.txt",
                           template = "AR6", summationsFile = summationFile, outputDirectory = tempdir(),
                           dataDumpFile = "checkSummations.csv"),
                   "1 equations are not satisfied")
    expect_false(all(tmp$diff == 0))
    expect_true(all(file.exists(file.path(tempdir(), c("log.txt", "checkSummations.csv")))))
  })
  unlink(file.path(tempdir(), c("test.mif", "log.txt", "checkSummations.csv")))
}
