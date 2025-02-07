for (piam_unit in c("US$2005", "US$2017")) {
  for (mif_unit in c("US$2005", "US$2017")) {
    test_that(paste0("checkFixUnits works with piam_unit=", piam_unit, " and mif_unit=", mif_unit), {
      # generate mapping file
      mappingstring <- c(
        "variable;unit;piam_variable;piam_unit;piam_factor",
        "Final Energy;EJ/yr;FE;EJ/yr;1",
        paste0("GDP;US$2017;GDP|PPP;", piam_unit, ";1")
      )
      mappingfile <- file.path(tempdir(), "mapping.csv")
      writeLines(mappingstring, mappingfile)
      # generate sample data
      unitvars <- c("Final Energy" = "EJ/yr", "GDP|PPP" = mif_unit)
      data <- tibble::tibble(
        model = "REMIND",
        scenario = "testthat",
        region = "USA",
        variable = rep(names(unitvars), 2),
        unit = rep(unname(unitvars), 2),
        year = c(rep(2005, length(unitvars)), rep(2010, length(unitvars))),
        value = seq(length(unitvars) * 2)
      ) %>% quitteSort()
      # no need to adapt FE data
      energy <- droplevels(filter(data, ! grepl("US$", .data$unit, fixed = TRUE)))
      expect_identical(energy, checkFixUnits(energy, getMapping(mappingfile)))

      gdp <- droplevels(filter(data, grepl("US$", .data$unit, fixed = TRUE)))
      if (piam_unit == "US$2005" && mif_unit == "US$2017") {
        # is not supported
        expect_error(checkFixUnits(gdp, getMapping(mappingfile)))
      } else if (piam_unit == "US$2017" && mif_unit == "US$2005") {
        # is automatically adapting GDP values
        expect_no_error(gdp17 <- checkFixUnits(gdp, getMapping(mappingfile)))
        expect_true(all(abs(gdp17$value - 1.231357 * gdp$value) < 0.00001))
        expect_true(all(gdp17$unit == piam_unit))
      } else {
        # no need to adapt anything
        expect_identical(gdp, checkFixUnits(gdp, getMapping(mappingfile)))
      }
    })
  }
}

for (piam_unit in c("1/billion US$2005", "1/billion US$2017")) {
  for (mif_unit in c("1/billion US$2005", "1/billion US$2017")) {
    test_that(paste0("checkFixUnits works with piam_unit=", piam_unit, " and mif_unit=", mif_unit), {
      # generate mapping file
      mappingstring <- c(
        "variable;unit;piam_variable;piam_unit;piam_factor",
        "Final Energy;EJ/yr;FE;EJ/yr;1",
        paste0("1/GDP;1/billion US$2017;GDP|PPP;", piam_unit, ";1")
      )
      mappingfile <- file.path(tempdir(), "mapping.csv")
      writeLines(mappingstring, mappingfile)
      # generate sample data
      unitvars <- c("Final Energy" = "EJ/yr", "GDP|PPP" = mif_unit)
      data <- tibble::tibble(
        model = "REMIND",
        scenario = "testthat",
        region = "USA",
        variable = rep(names(unitvars), 2),
        unit = rep(unname(unitvars), 2),
        year = c(rep(2005, length(unitvars)), rep(2010, length(unitvars))),
        value = seq(length(unitvars) * 2)
      ) %>% quitteSort()
      # no need to adapt FE data
      energy <- droplevels(filter(data, ! grepl("US$", .data$unit, fixed = TRUE)))
      expect_identical(energy, checkFixUnits(energy, getMapping(mappingfile)))

      gdp <- droplevels(filter(data, grepl("US$", .data$unit, fixed = TRUE)))
      if (piam_unit == "1/billion US$2005" && mif_unit == "1/billion US$2017") {
        # is not supported
        expect_error(checkFixUnits(gdp, getMapping(mappingfile)))
      } else if (piam_unit == "1/billion US$2017" && mif_unit == "1/billion US$2005") {
        # is automatically adapting GDP values
        expect_no_error(gdp17 <- checkFixUnits(gdp, getMapping(mappingfile)))
        expect_true(all(abs(gdp17$value - 1 / 1.231357 * gdp$value) < 0.00001))
        expect_true(all(gdp17$unit == piam_unit))
      } else {
        # no need to adapt anything
        expect_identical(gdp, checkFixUnits(gdp, getMapping(mappingfile)))
      }
    })
  }
}

for (piam_unit in c("1/US$2005", "1/US$2017")) {
  for (mif_unit in c("1/US$2005", "1/US$2017")) {
    test_that(paste0("checkFixUnits works with piam_unit=", piam_unit, " and mif_unit=", mif_unit), {
      # generate mapping file
      mappingstring <- c(
        "variable;unit;piam_variable;piam_unit;piam_factor",
        "Final Energy;EJ/yr;FE;EJ/yr;1",
        paste0("1/GDP;1/US$2017;GDP|PPP;", piam_unit, ";1")
      )
      mappingfile <- file.path(tempdir(), "mapping.csv")
      writeLines(mappingstring, mappingfile)
      # generate sample data
      unitvars <- c("Final Energy" = "EJ/yr", "GDP|PPP" = mif_unit)
      data <- tibble::tibble(
        model = "REMIND",
        scenario = "testthat",
        region = "USA",
        variable = rep(names(unitvars), 2),
        unit = rep(unname(unitvars), 2),
        year = c(rep(2005, length(unitvars)), rep(2010, length(unitvars))),
        value = seq(length(unitvars) * 2)
      ) %>% quitteSort()
      # no need to adapt FE data
      energy <- droplevels(filter(data, ! grepl("US$", .data$unit, fixed = TRUE)))
      expect_identical(energy, checkFixUnits(energy, getMapping(mappingfile)))

      gdp <- droplevels(filter(data, grepl("US$", .data$unit, fixed = TRUE)))
      if (piam_unit == "1/US$2005" && mif_unit == "1/US$2017") {
        # is not supported
        expect_error(checkFixUnits(gdp, getMapping(mappingfile)))
      } else if (piam_unit == "1/US$2017" && mif_unit == "1/US$2005") {
        # is automatically adapting GDP values
        expect_no_error(gdp17 <- checkFixUnits(gdp, getMapping(mappingfile)))
        expect_true(all(abs(gdp17$value - 1 / 1.231357 * gdp$value) < 0.00001))
        expect_true(all(gdp17$unit == piam_unit))
      } else {
        # no need to adapt anything
        expect_identical(gdp, checkFixUnits(gdp, getMapping(mappingfile)))
      }
    })
  }
}
