for (template in c(setdiff(names(templateNames()), c("AR6", "NAVIGATE")),
                   list(c("AR6", "AR6_NGFS")), list(c("NAVIGATE", "SHAPE")))) {
  test_that(paste("test generateIIASASubmission with", paste(template, collapse = ",")), {
    vars <- NULL
    for (i in unlist(template)) {
      templateData <- getTemplate(i)
      vars <- c(vars, paste0(templateData$piam_variable, " (",
                             templateData$piam_unit, ")")[! is.na(templateData$piam_variable)])
    }
    data <- magclass::new.magpie(cells_and_regions = "GLO", years = seq(2005, 2100, 5), fill = 1, names = unique(vars))
    magclass::getSets(data)[3] <- "variable"
    data <- magclass::add_dimension(data, dim = 3.1, add = "model", nm = "REMIND")
    data <- magclass::add_dimension(data, dim = 3.1, add = "scenario", nm = "default")
    magclass::write.report(data, file = file.path(tempdir(), "test.mif"), ndigit = 0)
    generateMappingfile(templates = unlist(template), outputDir = tempdir(), fileName = "mapping.csv")
    expect_warning(generateIIASASubmission(tempdir(), model = "REMIND", mapping = file.path(tempdir(), "mapping.csv"),
                                          outputDirectory = tempdir(), logFile = file.path(tempdir(), "missing.log"),
                                          outputFilename = "submission.mif", generateSingleOutput = TRUE), regexp = NA)
    expectedFiles <- file.path(tempdir(), c("mapping.csv", "submission.mif", "missing.log", "test.mif"))
    expect_true(all(file.exists(expectedFiles)))
    unlink(expectedFiles)
  })
}
