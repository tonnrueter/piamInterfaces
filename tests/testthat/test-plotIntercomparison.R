test_that("plotComparison works", {
    lPV <- c("Temperature|Global Mean")
    tmpdir <- tempdir()
    data <- dplyr::filter(quitte::quitte_example_dataAR6,
                          .data$model != "GCAM",
                          ! .data$variable %in% c("Population", "Final Energy|Industry|Liquids"))
    capture.output(m <- capture_messages(
      plotIntercomparison(data,
                          outputDirectory = tmpdir,
                          lineplotVariables = lPV)))
    expect_match(m, "Add plot for Final Energy", all = FALSE)
    expect_match(m, "Add plot for Temperature", all = FALSE)
    expectedFiles <- c("compare_models_Delayed_transition.pdf", "compare_models_Current_Policies.pdf",
                       "compare_scenarios_REMIND.pdf", "compare_scenarios_MESSAGEix.pdf")
    for (ef in expectedFiles) {
      expect_true(file.exists(file.path(tmpdir, ef)))
      expect_true(file.info(file.path(tmpdir, ef))$size > 0)
    }
})
