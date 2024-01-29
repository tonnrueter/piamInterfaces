for (template in c(setdiff(names(templateNames()), c("AR6", "NAVIGATE", "AR6_NGFS", "SHAPE")),
                   list(c("AR6", "AR6_NGFS")), list(c("NAVIGATE", "SHAPE")))) {
  test_that(paste("test generateIIASASubmission with", paste(template, collapse = ",")), {
    data <- data.frame()
    for (i in unlist(template)) {
      templateData <- getTemplate(i)
      data <- rbind(data, select(templateData, c("variable" = "piam_variable", "unit" = "piam_unit")))
    }

    data <- data %>%
      filter(!is.na(variable)) %>%
      mutate(model = "REMIND", scenario = "default", region = "GLO", value = 1)

    data <- tidyr::crossing(data, year = seq(2005, 2030, 5))
    quitte::write.mif(x = as.quitte(data), path = file.path(tempdir(), "test.mif"))

    mappingFile <- file.path(tempdir(), "output", paste0("mapping_", paste0(template, collapse = "_"), ".csv"))
    if (any(template == "AR6")) {
      generateMappingfile(templates = unlist(template), outputDirectory = NULL,
                          fileName = mappingFile)
      expect_no_warning(generateIIASASubmission(file.path(tempdir(), "test.mif"), model = "MAgPIE",
                                                mappingFile = mappingFile,
                                                outputDirectory = file.path(tempdir(), "output"),
                                                logFile = file.path(tempdir(), "missing.log"),
                                                outputFilename = "submission.xlsx")
      )
    } else if (any(template == "CAMPAIGNers")) {
      expect_no_error(
        suppressWarnings( # CAMPAIGNers template has Moving Avg prices, but no Price|*|Rawdata variables
          generateIIASASubmission(
            tempdir(),
            model = "REMIND", mapping = unlist(template),
            outputDirectory = file.path(tempdir(), "output"),
            logFile = file.path(tempdir(), "missing.log"),
            outputFilename = "submission.mif"
          )
        )
      )
    } else {
      expect_no_warning(generateIIASASubmission(tempdir(), model = "REMIND", mapping = unlist(template),
                                                outputDirectory = file.path(tempdir(), "output"),
                                                logFile = file.path(tempdir(), "missing.log"),
                                                outputFilename = "submission.mif")
      )
    }
    expectedFiles <- file.path(
      tempdir(), c(file.path("output", if (any(template == "AR6")) "submission.xlsx" else "submission.mif"))
    )

    for (f in expectedFiles) expect_true(file.exists(f))
    unlink(expectedFiles)
  })
}

test_that("Correct Prices are selected and plusses ignored", {
  qe <- quitte::quitte_example_dataAR6
  vars <- c("Price|Secondary Energy|++|Electricity|Rawdata",
            "Price|Secondary Energy|++|Electricity|Moving Avg",
            "Price|Secondary Energy|++|Electricity",
            "Price|Secondary Energy|Gases",
            "Price|Secondary Energy|Gases|Moving Avg")
  levels(qe$variable)[seq_along(vars)] <- vars
  qe <- droplevels(dplyr::filter(qe, .data$variable %in% vars))
  levels(qe$unit) <- rep("US$2005/GJ", length(levels(qe$unit)))
  # just add 1 to first variable, 2 to second etc.
  for (v in seq_along(vars)) {
    qe$value[qe$variable == vars[[v]]] <- v
  }
  f <- file.path(tempdir(), "Pricecheck_AR6_1.mif")
  expect_no_warning(generateIIASASubmission(droplevels(filter(qe, grepl("Electricity", variable))),
                                            mapping = "AR6", outputDirectory = dirname(f),
                                            outputFilename = basename(f), logFile = file.path(tempdir(), "price.log")))
  expect_true(file.exists(f))
  qemif <- quitte::as.quitte(f)
  # you have to devide by 1.1 and round to compensate for inflation 2005 -> 2010
  peSeElec <- unique(filter(qemif, !!sym("variable") == "Price|Secondary Energy|Electricity")$value)
  expect_identical(round(peSeElec / 1.1), 3)

  # check whether results are identical if we remove the plus and don't write to file
  qenoplus <- qe
  levels(qenoplus$variable) <- removePlus(levels(qenoplus$variable))
  qenoplusmif <- expect_no_warning(droplevels(as.quitte(dplyr::as_tibble(
    generateIIASASubmission(droplevels(filter(qe, grepl("Electricity", variable))),
      mapping = "AR6", outputDirectory = NULL, outputFilename = NULL, logFile = NULL
    )
  ))))
  sortquitte <- function(d) {
    return(d[order(d$model, d$scenario, d$region, d$variable, d$period), ])
  }
  expect_true(all.equal(sortquitte(qemif), sortquitte(qenoplusmif)))

  f2 <- file.path(tempdir(), "Pricecheck_AR6_2.mif")
  # if Rawdata is not present, warn
  expect_warning(generateIIASASubmission(droplevels(filter(qe, grepl("Gases", variable))),
                                         mapping = "AR6", outputDirectory = dirname(f2),
                                         outputFilename = basename(f2), logFile = file.path(tempdir(), "price.log")),
                 "Your data contains no Price|*|Rawdata variables.")
  expect_true(file.exists(f2))
  qemif2 <- quitte::as.quitte(f2)
  peSeGas <- unique(filter(qemif2, !!sym("variable") == "Price|Secondary Energy|Gases|Natural Gas")$value)
  expect_identical(round(peSeGas / 1.1), 4)
})
