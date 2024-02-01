test_that("checkUnitFactor works", {

  # these should fail

  mapping <- data.frame(Variable = character(), Unit = character(),
                        piam_unit = character(), piam_factor = character())
  mapping[nrow(mapping) + 1, ] <- c("Population", "million", "billion", "100")
  expect_error(checkUnitFactor(mapping, logFile = NULL))

  mapping <- data.frame(Variable = character(), Unit = character(),
                        piam_unit = character(), piam_factor = character())
  mapping[nrow(mapping) + 1, ] <- c("Emissions|N2O", "kt N2O/yr", "Mt N2O/yr", "100")
  expect_error(checkUnitFactor(mapping, logFile = NULL))

  mapping <- data.frame(Variable = character(), Unit = character(),
                        piam_unit = character(), piam_factor = character())
  mapping[nrow(mapping) + 1, ] <- c("GDP", "US$2010", "US$2005", "110.774")
  expect_error(checkUnitFactor(mapping, logFile = NULL))

  mapping <- data.frame(Variable = character(), Unit = character(),
                        piam_unit = character(), piam_factor = character())
  mapping[nrow(mapping) + 1, ] <- c("Trade", "US$2010", "US$2005", NA)
  expect_error(checkUnitFactor(mapping, logFile = NULL))

  mapping <- data.frame(Variable = character(), Unit = character(),
                        piam_unit = character(), piam_factor = character())
  mapping[nrow(mapping) + 1, ] <- c("Trade", "EUR_2020", "US$2005", NA)
  expect_error(checkUnitFactor(mapping, logFile = NULL))

  # these are correct

  mapping <- data.frame(Variable = character(), Unit = character(),
                        piam_unit = character(), piam_factor = character())
  mapping[nrow(mapping) + 1, ] <- c("Population", "million", "billion", "1000")
  mapping[nrow(mapping) + 1, ] <- c("Emissions|N2O", "kt N2O/yr", "Mt N2O/yr", "1000")
  mapping[nrow(mapping) + 1, ] <- c("FE", "MJ", "GJ", "1000")
  mapping[nrow(mapping) + 1, ] <- c("GDP", "US$2010", "US$2005", "1.10774")
  mapping[nrow(mapping) + 1, ] <- c("Trade", "EUR_2017", "US$2005", "1.17")
  expect_no_error(checkUnitFactor(mapping, logFile = NULL))
})

for (t in names(templateNames())) {
  test_that(paste("checkUnitFactor for template", t), {
    expect_no_error(checkUnitFactor(getTemplate(t), logFile = NULL))
  })
}
