for (template in names(templateNames())) {
  test_that(paste("test generateIIASASubmission with", template), {
    templateData <- getTemplate(template)
    variables <- unique(paste0(templateData$r30m44, " (", templateData$r30m44_unit, ")")[! is.na(templateData$r30m44)])
    data <- magclass::new.magpie(cells_and_regions = "GLO", years = seq(2005, 2150, 5), fill = 1, names = variables)
    magclass::getSets(data)[3] <- "variable"
    data <- magclass::add_dimension(data, dim = 3.1, add = "model", nm = "REMIND")
    data <- magclass::add_dimension(data, dim = 3.1, add = "scenario", nm = "default")
    magclass::write.report(data, file = file.path(tempdir(), "test.mif"), ndigit = 0)
    generateMappingfile(templates = template, outputDir = tempdir(), fileName = "mapping.csv", commentFileName = NULL)
    expect_silent(generateIIASASubmission(tempdir(), model = "REMIND", mapping = file.path(tempdir(), "mapping.csv"),
                                          outputDirectory = tempdir(), logFile = file.path(tempdir(), "missing.log"),
                                          outputFilename = "submission.mif", generateSingleOutput = TRUE))
    expectedFiles <- file.path(tempdir(), c("mapping.csv", "submission.mif", "missing.log", "test.mif"))
    expect_true(all(file.exists(expectedFiles)))
    unlink(expectedFiles)
  })
}
