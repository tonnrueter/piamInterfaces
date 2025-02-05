
allMappings <- NULL # used to concatenate all mappings and see whether that works
minimalLength <- list("AR6" = 1900, "NAVIGATE" = 1900)
for (mapping in names(mappingNames())) {
  test_that(paste("checks on mapping", mapping), {
    expect_silent(allMappings <- rbind(allMappings, getMapping(mapping, requiredColsOnly = TRUE)))
    expect_silent(mappingData <- getMapping(mapping))
    expect_true(all(c("variable", "unit", "piam_variable", "piam_unit", "piam_factor") %in% names(mappingData)))
    expect_true(class(mappingData) == "data.frame")
    expect_true(length(mappingData$variable) > max(0, unlist(minimalLength[mapping])))
    # look for empty cells in variable column
    expect_true(sum(is.na(mappingData$variable)) == 0)

    # look for merge conflicts
    conflictsigns <- grep("===|<<<|>>>", mappingData[, 1], value = TRUE)
    if (length(conflictsigns) > 0) {
      warning("Lines that look like merge conflicts:\n", paste(conflictsigns, collapse = "\n"))
    }
    expect_true(length(conflictsigns) == 0, label = paste0(mapping, " has no merge conflicts"))

    # check whether piam_factor column has , as decimal separator
    factorWithComma <- mappingData %>%
      filter(grepl(",", .data$piam_factor)) %>%
      pull("variable")
    if (length(factorWithComma) > 0) {
      warning("These variables in mapping ", mapping, " have a piam_factor using a ',' as decimal. Please use '.':\n",
              paste(factorWithComma, collapse = "\n"),
              "\nYou can run: devtools::load_all(); write.csv2(getMapping('", mapping,
              "') %>% mutate(piam_factor = gsub(',', '.', .data$piam_factor)), mappingNames('", mapping,
              "'), na = '', row.names = FALSE, quote = FALSE)")
    }
    expect_length(factorWithComma, 0)

    # look for Moving Avg prices in REMIND variables
    movingavg <- mappingData %>%
      filter(grepl("^Price\\|.*\\|Moving Avg", .data$piam_variable),
             ifelse("source" %in% colnames(.data), .data$source %in% c("R", "Rx"), TRUE)) %>%
      pull("variable")
    if (length(movingavg) > 0) {
      warning("These variables use 'Price|*|Moving Avg' which is deprecated since remind2 1.111.0 on 2023-05-26.\n",
              "Please remove '|Moving Avg' to use a fixed moving average:\n", paste(movingavg, collapse = ", "))
    }
    expect_length(movingavg, 0)

    # check for duplicated rows
    duplicates <- mappingData %>%
      select("variable", "piam_variable") %>%
      mutate(piam_variable = deletePlus(.data$piam_variable),
             variable = deletePlus(.data$variable))
    duplicates <- filter(duplicates, duplicated(duplicates))
    if (nrow(duplicates) > 0) {
      warning("Duplicated line in ", mapping, ":\n",
              paste0(duplicates$variable, ";", duplicates$piam_variable, collapse = "\n"))
    }
    expect_equal(nrow(duplicates), 0)

    # check for |+| notation used inconsistenly
    vars <- setdiff(unique(mappingData$piam_variable), NA)
    somePlusSomeNot <- vars[table(deletePlus(unique(vars)))[deletePlus(vars)] > 1]
    if (length(somePlusSomeNot) > 0) {
      warning("Inconsistent use of |+| notation for these variables:\n",
              paste(somePlusSomeNot, collapse = ", "))
    }
    expect_length(somePlusSomeNot, 0)

    # check for inconsistent variable + unit combinations
    nonempty <- dplyr::filter(mappingData, ! is.na(.data$piam_variable))
    allVarUnit <- unitjoin(nonempty$piam_variable, nonempty$piam_unit)
    unclearVar <- nonempty$piam_variable[duplicated(nonempty$piam_variable) & ! duplicated(allVarUnit)]
    unclearVarUnit <- sort(unique(allVarUnit[nonempty$piam_variable %in% unclearVar]))
    if (length(unclearVarUnit) > 0) {
      warning("Inconsistent units found for piam_variable:\n",
              paste(unclearVarUnit, collapse = "\n"))
    }
    expect_true(length(unclearVarUnit) == 0, label = paste("PIAM variables and units are consistent for", mapping))

    allVarUnit <- unitjoin(mappingData$variable, mappingData$unit)
    unclearVar <- mappingData$variable[duplicated(mappingData$variable) & ! duplicated(allVarUnit)]
    unclearVarUnit <- sort(unique(allVarUnit[mappingData$variable %in% unclearVar]))
    if (length(unclearVarUnit) > 0) {
      warning("Inconsistent units found for project variable:\n",
              paste(unclearVarUnit, collapse = "\n"))
    }
    expect_true(length(unclearVarUnit) == 0, label = paste("variables and units are consistent for", mapping))

    unitfails <- mappingData %>%
      filter(! checkUnitFactor(mappingData)) %>%
      select(c("variable", "unit", "piam_variable", "piam_unit", "piam_factor")) %>%
      arrange(.data$variable)
    if (nrow(unitfails) > 0) {
      printoutput <- withr::with_options(list(width = 180), print(unitfails, n = 200, na.print = "")) %>%
        capture.output()
      warning("Unknown unit conversion in ", mapping, ".\nFix it, or if that is a false positive, ",
              "adjust areUnitsIdentical() or checkUnitFactor(), see README.md\n",
              paste(printoutput, collapse = "\n"))
    }
    expect_true(nrow(unitfails) == 0)

    # check if piam_factor is supplied without variable
    factorWithoutVar <- mappingData %>%
      filter(! is.na(.data$piam_factor), is.na(.data$piam_variable)) %>%
      pull("variable")
    if (length(factorWithoutVar) > 0) {
      warning("These variables in mapping ", mapping, " have a piam_factor, but nothing specified in piam_variable:\n",
              paste(factorWithoutVar, collapse = "\n"))
    }
    expect_length(factorWithoutVar, 0)

    # checks only if source is supplied
    if ("source" %in% colnames(mappingData)) {
      # check for empty piam_variable with source
      sourceWithoutVar <- mappingData %>%
        filter(! is.na(.data$source), is.na(.data$piam_variable)) %>%
        pull("variable")
      if (length(sourceWithoutVar) > 0) {
        warning("These variables in mapping ", mapping, " have a non-empty source column, ",
                "but nothing specified in piam_variable:\n", paste(sourceWithoutVar, collapse = "\n"))
      }
      expect_length(sourceWithoutVar, 0)

      # check for piam_variable without source if source is supplied
      varWithoutSource <- mappingData %>%
        filter(! is.na(.data$piam_variable), is.na(.data$source)) %>%
        pull("piam_variable") %>%
        unique()
      if (length(varWithoutSource) > 0) {
        warning("These piam_variable in mapping ", mapping, " have an empty source column, see tutorial.md:\n",
                paste(varWithoutSource, collapse = "\n"))
      }
      expect_length(varWithoutSource, 0)

      # add here once you added new options to the tutorial
      sources <- read.csv2(system.file("sources.csv", package = "piamInterfaces"))
      sourceOptions <- sort(c(sources$symbol, paste0(sources$symbol, "x")))
      unknownSource <- mappingData %>% pull("source") %>% unique() %>% setdiff(c(NA, sourceOptions))
      if (length(unknownSource) > 0) {
        warning("The source column of mapping ", mapping, " contains these unknown options:\n",
                paste0(unknownSource, collapse = ", "),
                ".\nThe only options are ", paste0(sources$symbol, " (", sources$model, ")", collapse = ", "), ".\n",
                "New options should be explained in 'tutorial.md' and added to 'inst/sources.csv'.")
      }
      expect_length(unknownSource, 0)
    } # end source checks

    testfile <- file.path(tempdir(), paste0("mapping_", mapping, ".csv"))
    writeMapping(mappingData, testfile)
    mappingAfterWriting <- getMapping(testfile)
    expect_identical(mappingData, mappingAfterWriting)
  })
}
