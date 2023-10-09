test_that("fixOnRef works", {
  qe <- quitte::quitte_example_dataAR6
  d <- droplevels(dplyr::filter(qe,
                                scenario == first(scenario),
                                model == first(model)))

  expect_true(fixOnRef(d, d, startyear = 2020, ret = "boolean"))
  # somehow, only MESSAGEix results are correct in the dataset above
  expect_true(fixOnRef(filter(qe, model == "MESSAGEix"), "Current Policies", startyear = 2020, ret = "boolean"))
  qefixed <- fixOnRef(qe, "Current Policies", startyear = 2020, ret = "fixed")
  expect_true(fixOnRef(qefixed, "Current Policies", startyear = 2020, ret = "boolean"))
  expect_identical(fixOnRef(d, d, startyear = 2020, ret = "fixed"), d)
  expect_true(is.null(fixOnRef(d, d, startyear = 2020, ret = "fails")))
  # generate object with wrong data in 2020
  dwrong <- mutate(d, value = ifelse(period == 2020, 2 * value, value))
  expect_true(fixOnRef(dwrong, d, startyear = 2020, ret = "boolean"))
  expect_false(fixOnRef(dwrong, d, startyear = 2025, ret = "boolean"))
  # adjust only one variable
  dwrong <- mutate(d, value = ifelse(period == 2020 & variable == "Population", 0, value))
  expect_true(is.null(fixOnRef(dwrong, d, startyear = 2020, ret = "fails")))
  data <- fixOnRef(dwrong, d, startyear = 2025, ret = "fails")
  expect_true("Population" %in% levels(data$variable))
  expect_true(length(levels(data$variable)) == 1)
  expect_identical(quitteSort(d), quitteSort(fixOnRef(dwrong, d, startyear = 2025, ret = "fixed")))
  expect_identical(dwrong, fixOnRef(dwrong, d, startyear = 2010, ret = "fixed"))
  expect_true(fixOnRef(dwrong, d, startyear = min(dwrong$period), ret = "boolean"))
  # make sure only valid settings are accepted
  expect_no_error(fixOnRef(qe, filter(qe, scenario == first(scenario)), startyear = 2005, ret = "boolean"))
  expect_error(fixOnRef(dwrong, d, startyear = 2020, ret = "whatever"), "ret")
  expect_error(fixOnRef(dwrong, d, startyear = "whenever"), "startyear")
  expect_error(fixOnRef(qe, qe, startyear = 2020), "refscen")
})
