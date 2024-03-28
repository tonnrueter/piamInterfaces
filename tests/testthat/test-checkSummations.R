test_that("checkSummations works", {
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
                                              dataDumpFile = "checkSummations1.csv", absDiff = 0, relDiff = 0),
                       "All summation checks were fine")
        tmp <- droplevels(filter(tmp, !is.na(tmp$value)))
        expect_true(file.exists(file.path(tempdir(), "checkSummations1.csv")))
        expect_true(all(tmp$diff == 0))
        expect_true(nrow(tmp) > 0)
        expect_true(any(grepl("^Final Energy\\|Industry( [0-9]+)?$", unique(tmp$variable))))
      } else {
        expect_message(tmp <- checkSummations(mifFile = file.path(tempdir(), "test.mif"), logFile = NULL,
                                              template = summationFile, summationsFile = summationFile,
                                              outputDirectory = tempdir(),
                                              dataDumpFile = "checkSummations2.csv"),
                       "All summation checks were fine")
        expect_true(file.exists(file.path(tempdir(), "checkSummations2.csv")))
        tmp <- droplevels(filter(tmp, !is.na(tmp$value)))
        expect_true(nrow(tmp) == 0)
      }
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
                                                             dataDumpFile = "checkSummations4.csv",
                                                             generatePlots = TRUE,
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

  # test usage of same variables multiple times as a child in summation groups ----
  # Capacity|Electricity|Oil = Capacity|Electricity|Oil|w/o CCS + Capacity|Electricity|Oil|w/ CCS
  # Capacity|Electricity" = ... + Capacity|Electricity|Nuclear, Capacity|Electricity|Oil|w/o CCS
  varnames <- paste(c("Capacity|Electricity|Oil", "Capacity|Electricity|Oil|w/o CCS", "Capacity|Electricity|Oil|w/ CCS",
                      "Capacity|Electricity", "Capacity|Electricity|Nuclear"),
                    "(EJ/yr)")

  data <- magclass::new.magpie(cells_and_regions = "GLO", years = c(2030), fill = c(10, 5, 5, 7, 2),
                               names = varnames)

  magclass::getSets(data)[3] <- "variable"
  sumChecks <- checkSummations(mifFile = data, outputDirectory = NULL, summationsFile = "AR6") %>%
    filter(diff != 0)
  expect_true(nrow(sumChecks) == 0)

  # test usage of different summations for same variable ----
  # Final Energy = .. + Final Energy|Electricity + Final Energy|Gases
  # Final Energy 2 = .. + Final Energy|Industry + Final Energy|Transportation

  varnames <- paste(c("Final Energy", "Final Energy|Industry", "Final Energy|Transportation",
                      "Final Energy|Electricity", "Final Energy|Gases"),
                    "(EJ/yr)")

  data <- magclass::new.magpie(cells_and_regions = "GLO", years = c(2030), fill = c(10, 5, 6, 7, 3),
                               names = varnames)
  magclass::getSets(data)[3] <- "variable"
  sumChecks <- checkSummations(mifFile = data, outputDirectory = NULL, summationsFile = "AR6") %>%
    filter(diff != 0)
  expect_true(nrow(sumChecks) == 1)
  expect_true(unique(sumChecks$variable) == "Final Energy 2")

  # test extractVariableGroups option, testing a magclass object  ----
  varnames <- paste(c("FE|Industry|Steel", "FE|Industry|Steel|+|Primary", "FE|Industry|Steel|+|Secondary"),
                    "(EJ/yr)")
  data <- magclass::new.magpie(cells_and_regions = "GLO", years = c(2030, 2050), fill = c(2, 4, 1, 2, 1, 2),
                               names = varnames)
  magclass::getSets(data)[3] <- "variable"
  sumChecks <- checkSummations(mifFile = data, outputDirectory = NULL, summationsFile = "extractVariableGroups") %>%
    filter(diff != 0)
  expect_true(nrow(sumChecks) == 0)

  dataerror <- magclass::new.magpie(cells_and_regions = "GLO", years = c(2030, 2050), fill = c(2, 4, 1, 1, 1, 1),
                                    names = varnames)
  magclass::getSets(dataerror)[3] <- "variable"
  sumChecks <- dataerror %>%
    checkSummations(outputDirectory = NULL, summationsFile = "extractVariableGroups") %>%
    filter(.data$diff != 0)
  expect_true(nrow(sumChecks) == 1)
})
