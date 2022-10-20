test_that("test generateMappingfile", {
  for (template in templateNames()) {
    expect_silent(mapping <- generateMappingfile(templates = template, outputDir = tempdir(),
                                                 fileName = NULL, commentFileName = NULL))
    expect_silent(mapping <- generateMappingfile(templates = template, outputDir = tempdir(),
                                                 fileName = "mapping.csv", commentFileName = NULL))
    expect_true(file.exists(file.path(tempdir(), "mapping.csv")))
    unlink(file.path(tempdir(), "mapping.csv"))
  }
})
