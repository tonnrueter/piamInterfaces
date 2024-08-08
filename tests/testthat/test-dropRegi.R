test_that(".dropRegi works", {
  regiEU21 <- c("DEU", "ECE", "ECS", "ENC", "ESC", "ESW", "EWN", "FRA", "UKI", "NEN", "NES")
  mifdata <- quitte::as.quitte(data.frame(
    "model" = "REMIND",
    "scenario" = "Base",
    "variable" = "FE",
    "value" = seq(15),
    "period" = 2005,
    "region" = c("USA", "EUR", "NEU", "GLO", regiEU21),
    "unit" = "EJ/yr"
  ))
  expect_identical(mifdata, .dropRegi(mifdata, NULL))
  expect_identical(mifdata, .dropRegi(mifdata, "nonexistent"))
  mifdataNoENC <- droplevels(filter(mifdata, ! .data$region %in% "ENC"))
  expect_identical(mifdataNoENC, .dropRegi(mifdataNoENC, "auto"))
  expect_message(removedAggregates <- .dropRegi(mifdata, "auto"), "Dropping those regions")
  expect_setequal(levels(removedAggregates$region), setdiff(levels(mifdata$region), c("EUR", "NEU")))
})
