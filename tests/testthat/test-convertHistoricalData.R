test_that("convertHistoricalData works with REMIND_2_R10", {
  REMINDregi <- c("EUR", "LAM", "CHA", "IND", "OAS", "SSA", "REF", "NEU", "USA", "MEA", "JPN", "CAZ", "World")
  d <- as.quitte(data.frame(model = "REMIND", scenario = "default", region = REMINDregi,
                 period = 2030, variable = "FE", value = 1, unit = "EJ/yr"))
  expect_silent(p <- as.quitte(convertHistoricalData(d, project = "AR6", regionMapping = "REMIND_2_R10")))
  expect_true(all(grepl(" \\(R10\\)|^World$", levels(p$region))))
  expect_true(all(p$value %in% c(1, 2)))
  expect_equal(length(levels(p$region)), 12) # R10 + Other + World
})

test_that("convertHistoricalData works with ISO_2_R5", {
  regi <- c("DEU", "FRA")
  d <- as.quitte(data.frame(model = "REMIND", scenario = "default", region = regi,
                 period = 2030, variable = "FE", value = 1, unit = "EJ/yr"))
  expect_silent(p <- as.quitte(convertHistoricalData(d, project = "AR6", regionMapping = "ISO_2_R5")))
  expect_true(all(grepl(" \\(R5\\)|^World$", levels(p$region))))
  expect_equal(p$value, 2)
  expect_equal(levels(p$model), "REMIND")
  expect_equal(levels(p$scenario), "default")
  expect_equal(p$period, 2030)
  expect_equal(levels(p$region), "OECD & EU (R5)")
})

