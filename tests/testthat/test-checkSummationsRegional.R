test_that("checkSummations works", {

  variables <- c("Final Energy", "Price|Final Energy")
  mifdata <- quitte::as.quitte(data.frame(
    "model" = "REMIND",
    "scenario" = "Base",
    "variable" = variables,
    "value" = c(4000, 5, 3000, 3, 1000, 6), # FE, P|FE, FE ...
    "period" = 2005,
    "region" = c("GLO", "GLO", "EUR", "EUR", "USA", "USA"),
    "unit" = c("EJ/yr", "US$2010/EJ")
  ))
  expect_equal(nrow(checkSummationsRegional(mifdata)), 0)

  mifdata <- quitte::as.quitte(data.frame(
    "model" = "REMIND",
    "scenario" = "Base",
    "variable" = variables,
    "value" = c(10000, 10, 3000, 3, 1000, 6), # FE, P|FE, FE ...
    "period" = 2005,
    "region" = c("GLO", "GLO", "EUR", "EUR", "USA", "USA"),
    "unit" = c("EJ/yr", "US$2010/EJ")
  ))
  expect_no_warning(d <- checkSummationsRegional(mifdata))
  expect_equal(nrow(d), 2)
  expect_true(all(variables %in% levels(d$variable)))

  extensive <- filter(d, .data$variable %in% "Final Energy")
  expect_true(all(is.na(extensive$max)))
  expect_true(all(is.na(extensive$min)))
  expect_true(all(! is.na(extensive$checkSum)))

  intensive <- filter(d, .data$variable %in% "Price|Final Energy")
  expect_true(all(! is.na(intensive$max)))
  expect_true(all(! is.na(intensive$min)))
  expect_true(all(is.na(intensive$checkSum)))
  expect_true(all(intensive$total > intensive$max | intensive$total < intensive$min))
  expect_true(all(is.na(intensive$checkSum)))
})
