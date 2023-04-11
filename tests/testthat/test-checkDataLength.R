test_that("checkDataLength works", {
  mifdata <- quitte::quitte_example_dataAR6
  expect_silent(checkDataLength(mifdata))

  mifdata <- filter(mifdata, !!sym("variable") == "Population" & !!sym("scenario") == "Current Policies")
  capture.output(expect_output(checkDataLength(mifdata),
                               "variables found whose data points differ between scenarios"))
})
