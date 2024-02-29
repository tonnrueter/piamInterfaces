test_that("checkIIASATemplate works", {

  # check that quitte_example_dataAR6 runs through without a problem
  iiasaxlsx <- file.path(tempdir(), "iiasatemplate.xlsx")
  writexl::write_xlsx(x = qeAR6, path = iiasaxlsx)

  mifdata <- qeAR6
  capture.output(miftest <- checkIIASASubmission(mifdata, iiasaxlsx, failOnUnitMismatch = TRUE))
  expect_identical(mifdata, miftest)

  # generate unit mismatch fail
  mifdata <- qeAR6
  levels(mifdata$unit)[4] <- "zillion"
  expect_error(capture.output(checkIIASASubmission(mifdata, iiasaxlsx, failOnUnitMismatch = TRUE)),
               "Unit mismatches")

  # generate data length warning message
  mifdata <- qeAR6 %>%
    filter(.data$variable == "Population") %>%
    filter(.data$scenario %in% c("Current Policies", "Delayed transition")) %>%
    filter(.data$period < 2050 | .data$scenario %in% "Current Policies")
  capture.output(expect_output(checkIIASASubmission(mifdata, iiasaxlsx, failOnUnitMismatch = FALSE),
                               "variables found whose data points differ between scenarios"))

  # further ideas for tests?
  templatedata <- tibble::as_tibble(data.frame(
    "variable" = c("Price|Energy", "Price|Energy|Index", "Population"),
    "unit" = c("US$2010/GJ", "Index (2005 = 1)", "million")
  ))

  mifdata <- quitte::as.quitte(data.frame(
    "model" = "REMIND",
    "scenario" = c("Base", "Base", "NDC", "NDC"),
    "variable" = "Price|Energy",
    "value" = c(2., 6., 5., 8.),
    "period" = c(2005, 2010, 2005, 2010),
    "region" = "GLO",
    "unit" = "US$2010/GJ"
  ))



})
