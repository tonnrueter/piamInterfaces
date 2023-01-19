test_that("plotComparison works", {
    lPV <- c("Temperature|Global Mean", "Population")
    tmpdir <- tempdir()
    m <- capture_messages(
      plotIntercomparison(quitte::quitte_example_dataAR6,
                          outputDirectory = tmpdir,
                          lineplotVariables = lPV))
    expect_match(m, "Add area plot for Final Energy", all = FALSE)
    expect_match(m, "Add line plot for Temperature", all = FALSE)
    expect_match(m, "Add line plot for Population", all = FALSE)
    expect_true(file.exists(file.path(tmpdir, "compare_models_Delayed_transition.pdf")))
    expect_true(file.exists(file.path(tmpdir, "compare_models_Current_Policies.pdf")))
    expect_true(file.exists(file.path(tmpdir, "compare_scenarios_GCAM.pdf")))
    expect_true(file.exists(file.path(tmpdir, "compare_scenarios_REMIND.pdf")))
    expect_true(file.exists(file.path(tmpdir, "compare_scenarios_MESSAGEix.pdf")))
})
