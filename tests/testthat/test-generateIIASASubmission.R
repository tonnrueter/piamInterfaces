for (template in c(setdiff(names(templateNames()), "NAVIGATE"),
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
    # expect_warning with regexp = NA implies no warning
    if (all(template == "AR6")) {
      generateMappingfile(templates = unlist(template), outputDir = file.path(tempdir(), "output"),
                          fileName = "mapping_AR6.csv")
      expect_warning(generateIIASASubmission(tempdir(), model = "MAgPIE",
                                             mappingFile = file.path(tempdir(), "output", "mapping_AR6.csv"),
                                             outputDirectory = file.path(tempdir(), "output"),
                                             logFile = file.path(tempdir(), "missing.log"),
                                             outputFilename = "submission.mif", generateSingleOutput = TRUE),
                   regexp = NA)
    } else {
      expect_warning(generateIIASASubmission(tempdir(), model = "REMIND", mapping = template,
                                             outputDirectory = file.path(tempdir(), "output"),
                                             logFile = file.path(tempdir(), "missing.log"),
                                             outputFilename = "submission.mif", generateSingleOutput = TRUE),
                   regexp = NA)
    }
    expectedFiles <- file.path(tempdir(), c("missing.log", "test.mif",
                 file.path("output", c(paste0("mapping_", paste0(template, collapse = "_"), ".csv"),
                                       "submission.mif", "submission.xlsx"))))
    for (f in expectedFiles) expect_true(file.exists(f))
    unlink(expectedFiles)
  })
}
