for (template in c(setdiff(names(templateNames()), c("AR6", "NAVIGATE", "AR6_NGFS", "SHAPE")),
                   list(c("AR6", "AR6_NGFS")), list(c("NAVIGATE", "SHAPE")))) {
  test_that(paste("test generateIIASASubmission with", paste(template, collapse = ",")), {
    vars <- NULL
    for (i in unlist(template)) {
      templateData <- getTemplate(i)
      vars <- c(vars, paste0(templateData$piam_variable, " (",
                             templateData$piam_unit, ")")[! is.na(templateData$piam_variable)])
    }
    data <- magclass::new.magpie(cells_and_regions = "GLO", years = seq(2005, 2030, 5), fill = 1, names = unique(vars))
    magclass::getSets(data)[3] <- "variable"
    data <- magclass::add_dimension(data, dim = 3.1, add = "model", nm = "REMIND")
    data <- magclass::add_dimension(data, dim = 3.1, add = "scenario", nm = "default")
    magclass::write.report(data, file = file.path(tempdir(), "test.mif"), ndigit = 0)
    # expect_warning with regexp = NA implies no warning
    mappingFile <- file.path(tempdir(), "output", paste0("mapping_", paste0(template, collapse = "_"), ".csv"))
    if (any(template == "AR6")) {
      generateMappingfile(templates = unlist(template), outputDir = NULL,
                          fileName = mappingFile)
      expect_warning(generateIIASASubmission(file.path(tempdir(), "test.mif"), model = "MAgPIE",
                                             mappingFile = mappingFile,
                                             outputDirectory = file.path(tempdir(), "output"),
                                             logFile = file.path(tempdir(), "missing.log"),
                                             outputFilename = "submission.mif"),
                   regexp = NA)
    } else {
      expect_warning(generateIIASASubmission(tempdir(), model = "REMIND", mapping = unlist(template),
                                             outputDirectory = file.path(tempdir(), "output"),
                                             logFile = file.path(tempdir(), "missing.log"),
                                             outputFilename = "submission.mif"),
                   regexp = NA)
    }
    expectedFiles <- file.path(tempdir(), c(file.path("output", "submission.mif")))

    for (f in expectedFiles) expect_true(file.exists(f))
    unlink(expectedFiles)
  })
}
