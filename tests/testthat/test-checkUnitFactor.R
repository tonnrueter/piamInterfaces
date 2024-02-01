test_that("checkUnitFactor works", {

  # these should fail

  wrong <- list(
    c("Population", "million", "billion", "100"),
    c("Emissions|N2O", "kt N2O/yr", "Mt N2O/yr", "100"),
    c("GDP", "US$2010", "US$2005", "110.774"),
    c("Trade", "US$2010", "US$2005", NA),
    c("Trade", "EUR_2020", "US$2005", NA),
    c("Trade", "EUR_2020", "US$2005", "1.10")
  )
  correct <- list(
    c("Population", "million", "billion", "1000"),
    c("Emissions|N2O", "kt N2O/yr", "Mt N2O/yr", "1000"),
    c("FE", "MJ", "GJ", "1000"),
    c("GDP", "US$2010", "US$2005", "1.10774"),
    c("Trade", "EUR_2020", "US$2005", "1.17")
  )
  for (w in wrong) {
    mapping <- data.frame(Variable = character(), Unit = character(),
                          piam_unit = character(), piam_factor = character())
    mapping[1, ] <- w
    expect_error(checkUnitFactor(mapping, logFile = NULL))
  }
  for (c in correct) {
    mapping <- data.frame(Variable = character(), Unit = character(),
                          piam_unit = character(), piam_factor = character())
    mapping[1, ] <- c
    expect_no_error(checkUnitFactor(mapping, logFile = NULL))
  }
})

for (t in names(templateNames())) {
  test_that(paste("checkUnitFactor for template", t), {
    expect_no_error(checkUnitFactor(getTemplate(t), logFile = NULL))
  })
}
