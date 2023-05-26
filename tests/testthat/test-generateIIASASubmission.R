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
                                             outputFilename = "submission.xlsx"),
                   regexp = NA)
    } else {
      expect_warning(generateIIASASubmission(tempdir(), model = "REMIND", mapping = unlist(template),
                                             outputDirectory = file.path(tempdir(), "output"),
                                             logFile = file.path(tempdir(), "missing.log"),
                                             outputFilename = "submission.mif"),
                   regexp = NA)
    }
    expectedFiles <- file.path(
      tempdir(), c(file.path("output", if (any(template == "AR6")) "submission.xlsx" else "submission.mif")))

    for (f in expectedFiles) expect_true(file.exists(f))
    unlink(expectedFiles)
  })
}

test_that("Correct Prices are selected", {
  library(piamInterfaces)
  qe <- quitte::quitte_example_dataAR6
  # if Rawdata is present, use the value without anything. If not, use Moving Avg.
  vars <- c("Price|Secondary Energy|Electricity|Rawdata",
            "Price|Secondary Energy|Electricity|Moving Avg",
            "Price|Secondary Energy|Electricity",
            "Price|Secondary Energy|Gases",
            "Price|Secondary Energy|Gases|Moving Avg")
  levels(qe$variable)[seq_along(vars)] <- vars
  qe <- droplevels(dplyr::filter(qe, .data$variable %in% vars))
  levels(qe$unit) <- rep("US$2005/GJ", length(levels(qe$unit)))
  for (v in seq_along(vars)) {
    qe$value[qe$variable == vars[[v]]] <- v
  }
  f <- file.path(tempdir(), "Pricecheck_AR6.mif")
  expect_warning(generateIIASASubmission(qe, mapping = "AR6", outputDirectory = dirname(f),
                                         outputFilename = basename(f), logFile = file.path(tempdir(), "price.log")),
                                         regexp = NA)
  expect_true(file.exists(f))
  qemif <- quitte::as.quitte(f)
  # you have to devide by 1.1 and round to compensate for inflation 2005 -> 2010
  peSeElec <- unique(filter(qemif, !!sym("variable") == "Price|Secondary Energy|Electricity")$value)
  expect_true(round(peSeElec / 1.1) == 3)
  peSeGas <- unique(filter(qemif, !!sym("variable") == "Price|Secondary Energy|Gases|Natural Gas")$value)
  expect_true(round(peSeGas / 1.1) == 5)
})
