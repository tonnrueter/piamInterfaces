test_that("test generateMappingfile", {
  for (template in templateNames()) {
    # regexp = NA mains no warning expected
    expect_warning(mapping <- generateMappingfile(templates = template, outputDir = tempdir(),
                                                 fileName = NULL, logFile = NULL), regexp = NA)
    expect_warning(mapping <- generateMappingfile(templates = template, outputDir = tempdir(),
                                                 fileName = "mapping.csv", logFile = NULL), regexp = NA)
    expect_true(file.exists(file.path(tempdir(), "mapping.csv")))
    unlink(file.path(tempdir(), "mapping.csv"))
  }
})
