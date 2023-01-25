test_that("plotComparison works", {
    lPV <- c("Temperature|Global Mean")
    tmpdir <- tempdir()
    data <- dplyr::filter(quitte::quitte_example_dataAR6, .data$model != "GCAM")
    capture.output(m <- capture_messages(
      plotIntercomparison(data,
                          outputDirectory = tmpdir,
                          lineplotVariables = lPV)))
    expect_match(m, "Add plot for Final Energy", all = FALSE)
    expect_match(m, "Add plot for Temperature", all = FALSE)
    expect_true(file.exists(file.path(tmpdir, "compare_models_Delayed_transition.pdf")))
    expect_true(file.exists(file.path(tmpdir, "compare_models_Current_Policies.pdf")))
    expect_true(file.exists(file.path(tmpdir, "compare_scenarios_REMIND.pdf")))
    expect_true(file.exists(file.path(tmpdir, "compare_scenarios_MESSAGEix.pdf")))
})
