for (mapping in c(setdiff(names(mappingNames()), c("AR6", "NAVIGATE", "AR6_NGFS", "SHAPE")),
                  list(c("AR6", "AR6_NGFS")), list(c("NAVIGATE", "SHAPE")))) {
  test_that(paste("test generateIIASASubmission with", paste(mapping, collapse = ",")), {
    data <- data.frame()
    for (i in unlist(mapping)) {
      mappingData <- getTemplate(i)
      data <- rbind(data, select(mappingData, c("variable" = "piam_variable", "unit" = "piam_unit")))
    }

    data <- data %>% # [seq(min(10, nrow(data))), ] %>%
      filter(!is.na(.data$variable)) %>%
      mutate(model = "REMIND", scenario = "default", region = "GLO", value = 1) %>%
      mutate(variable = deletePlus(.data$variable)) %>%
      distinct(.data$variable, .keep_all = TRUE)

    data <- tidyr::crossing(data, year = seq(2005, 2020, 5))

    outputFilename <- if (any(mapping == "AR6")) "submission.xlsx" else "submission.mif"

    expect_no_warning(generateIIASASubmission(as.quitte(data), model = "REMIND", mapping = unlist(mapping),
                                              outputDirectory = file.path(tempdir(), "output"),
                                              logFile = file.path(tempdir(), "missing.log"),
                                              outputFilename = outputFilename,
                                              checkSummation = any(mapping == "NAVIGATE")),
      message = paste0("warnings for mapping(s)", paste(mapping, collapse = ","))
    )
    expectedFiles <- file.path(tempdir(), "output", outputFilename)

    for (f in expectedFiles) expect_true(file.exists(f))
    unlink(expectedFiles)
  })
}

test_that("Community template works", {
  templateurl <- "https://files.ece.iiasa.ac.at/common-definitions/common-definitions-template.xlsx"
  expect_no_warning(d <- generateIIASASubmission(quitte::quitte_example_data, mapping = "NAVIGATE",
                                                 outputDirectory = NULL, iiasatemplate = templateurl))
  expect_true(nrow(d) > 0)
})

test_that("Correct Prices are selected and plusses ignored", {
  qe <- qeAR6
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
                                            mapping = "AR6", outputDirectory = dirname(f), checkSummation = FALSE,
                                            outputFilename = basename(f), logFile = file.path(tempdir(), "price.log")))
  expect_true(file.exists(f))
  qemif <- quitte::as.quitte(f)
  # you have to divide by 1.1 and round to compensate for inflation 2005 -> 2010
  peSeElec <- unique(filter(qemif, !!sym("variable") == "Price|Secondary Energy|Electricity")$value)
  expect_identical(round(peSeElec / 1.1), 3)

  # check whether results are identical if we remove the plus and don't write to file
  qenoplus <- qe
  levels(qenoplus$variable) <- removePlus(levels(qenoplus$variable))
  expect_no_warning(qenoplusmif <- droplevels(as.quitte(dplyr::as_tibble(
    generateIIASASubmission(droplevels(filter(qe, grepl("Electricity", variable))),
      mapping = "AR6", outputDirectory = NULL, outputFilename = NULL, logFile = NULL, checkSummation = FALSE
    )
  ))))
  sortquitte <- function(d) {
    return(d[order(d$model, d$scenario, d$region, d$variable, d$period), ])
  }
  expect_true(all.equal(sortquitte(qemif), sortquitte(qenoplusmif)))

  f2 <- file.path(tempdir(), "Pricecheck_AR6_2.mif")
  # if Rawdata is not present, warn
  expect_warning(generateIIASASubmission(droplevels(filter(qe, grepl("Gases", variable))),
                                         mapping = "AR6", outputDirectory = dirname(f2), checkSummation = FALSE,
                                         outputFilename = basename(f2), logFile = file.path(tempdir(), "price.log")),
                 "Your data contains no Price|*|Rawdata variables.")
  expect_true(file.exists(f2))
  qemif2 <- quitte::as.quitte(f2)
  peSeGas <- unique(filter(qemif2, !!sym("variable") == "Price|Secondary Energy|Gases|Natural Gas")$value)
  expect_identical(round(peSeGas / 1.1), 4)
})

test_that("fail on duplicated data", {
  dupl <- rbind(testdata, testdata)
  expect_warning(generateIIASASubmission(dupl, mapping = "AR6", outputFilename = NULL, logFile = NULL),
                 "Duplicated data found")
  dupl <- rbind(testdata, mutate(testdata, variable = sub("|", "|++|", variable, fixed = TRUE)))
  expect_warning(generateIIASASubmission(dupl, mapping = "AR6", outputFilename = NULL, logFile = NULL),
                 "Duplicated data found")
})

test_that("interpolation for ScenarioMIP works as expected", {

  # this test assumes that Emi|BC is one of the variables that should
  # be interpolated as per the ScenarioMIP template

  data <- data.frame(
    variable = "Emi|BC", unit = "Mt BC/yr",
    model = "REMIND", scenario = "default", region = "GLO",
    period = c(2005, 2010, 2020),
    value = c(1, 10, 15)
  )

  result <- generateIIASASubmission(
    mifs = data,
    mapping = "ScenarioMIP",
    outputFilename = NULL
  )

  expect_true(all(seq(2005, 2020, 1) %in% unique(result$period)))
  expect_true(max(result$period) == 2020)

})

# Unit test to ensure functionality of weighted
# averages in the mapping. Generates a mapping file with
# 2 piam variables which are aggregated into an output variable
.weightedVariablesTest <- function(values, includeWeightInMap) {
  piamVariables <- c("Var|One", "Var|Two")
  piamWeights <- c("Area|One", "Area|Two")
  piamUnits <- c(rep("PiamUnit", 2), rep("WeightUnit", 2))
  outputVariables <- c("OutputVar")
  outputUnits <- c("OutputUnit")
  modelName <- "aModel"

  .getMapping <- function(includeWeightColumn = FALSE) {
    mapping <- data.frame(
      variable = outputVariables,
      unit = outputUnits,
      piam_variable = piamVariables,
      piam_unit = rep("PiamUnit", 2),
      piam_factor = c("1", "1000"),
      weight = piamWeights,
      comment = rep("", 2)
    )
    if (includeWeightColumn == FALSE) mapping <- mapping %>% select(- "weight")
    write.csv2(mapping, quote = FALSE, file = file.path(tempdir(), "test_mappings.csv"), row.names = FALSE)
    return(file.path(tempdir(), "test_mappings.csv"))
  }

  .getMif <- function(values) {
    scenario <- "scen1"
    region <- "CHN"
    names <- c("Model", "Scenario", "Region", "Variable", "Unit", "2010")
    for (i in 1:4){
      row <- list(modelName, scenario, region, c(piamVariables, piamWeights)[i], piamUnits[i], values[i])
      names(row) <- names
      if (i == 1) {
        runOutput <- data.frame(row, check.names = FALSE)
      } else {
        runOutput <- rbind(runOutput, data.frame(row, check.names = FALSE))
      }
    }
    write.csv2(runOutput, quote = FALSE, file = file.path(tempdir(), "test_mif.csv"), row.names = FALSE)
    return(file.path(tempdir(), "test_mif.csv"))
  }

  # apply the mapping
  piam <- expect_no_warning(generateIIASASubmission(
    mifs = .getMif(values),
    mapping = .getMapping(includeWeightColumn = includeWeightInMap),
    model = modelName,
    outputFilename = NULL,
    timesteps = "all"
  ))

  return(piam$value)
}

.expectedFrame <- function(values, includeWeightColumn) {
  if (includeWeightColumn) {
    totalWeight <- values[3] + values[4]
    out <- values[1] * (values[3] / totalWeight) + 1000 * values[2] * (values[4] / totalWeight)
  } else {
    out <- values[1] + 1000.0 * values[2]
  }
  outputRows <- data.frame(
    variable = "OutputVar",
    value = out,
    model = "aModel",
    scenario = "scen1",
    region = "CHN",
    unit = "OutputUnit"
  )
  return(outputRows)
}


test_that("Mapping handles weights as expected.", {
  # When all values present, with either a weight column included or not
  expect_equal(
    .weightedVariablesTest(c(10, 10, 10, 10), includeWeightInMap = FALSE),
    10 + 1000 * 10
  )
  expect_equal(
    .weightedVariablesTest(c(10, 10, 10, 10), includeWeightInMap = TRUE),
    10 * (10 / 20) + 1000 * 10 * (10 / 20)
  )
  # When a value is missing from the mif. If its a weighted variable then
  # omit it from the weighted sum. therefore expect the total weight to 10
  # not 20
  expect_equal(
    .weightedVariablesTest(c(NA, 10, 10, 10), includeWeightInMap = FALSE),
    1000 * 10
  )
  expect_equal(
    .weightedVariablesTest(c(NA, 10, 10, 10), includeWeightInMap = TRUE),
    1000 * 10 * (10 / 10) # ignore first row.
  )
  # When a weight is missing drop that whole row from the weighted average.
  expect_equal(
    .weightedVariablesTest(c(10, 10, NA, 10), includeWeightInMap = FALSE),
    10 + 1000 * 10
  )
  expect_equal(
    .weightedVariablesTest(c(10, 10, 10, NA), includeWeightInMap = TRUE),
    10 * (10 / 10) # if the weight is NA then ignore that from the total weight
  )
}
)
