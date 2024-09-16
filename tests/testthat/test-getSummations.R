test_that("basic checks on summation Groups", {
  for (template in names(summationsNames())) {
    expect_silent(summationsData <- getSummations(template))
    duplicates <- select(summationsData, "parent", "child")
    duplicates <- filter(duplicates, duplicated(duplicates))
    if (nrow(duplicates) > 0) {
      warning("Duplicated line in ", template, ":\n", paste0(duplicates$parent, ";", duplicates$child, collapse = "\n"))
    }
    expect_equal(nrow(duplicates), 0)
    expect_true(all(c("parent", "child", "factor") %in% names(summationsData)))
    expect_true(class(summationsData) == "data.frame")
    mappingData <- try(getMapping(template))
    if (! inherits(mappingData, "try-error") && ! template %in% "ECEMF") {
      notinMapping <- setdiff(c(gsub(" [1-9]$", "", summationsData$parent), summationsData$child), mappingData$variable)
      if (length(notinMapping) > 0) {
        warning("These elements of summation_groups_", template, " are not defined in mapping_", template, ": ",
                paste(notinMapping, collapse = ", "))
      }
      expect_equal(length(notinMapping), 0)
    }
  }
})

test_that("getSummations works from community template (errors might be caused by changes in the template)", {
  templateurl <- "https://files.ece.iiasa.ac.at/common-definitions/common-definitions-template.xlsx"
  expect_silent(summationsData <- getSummations(templateurl))
  expect_equal(names(summationsData), c("parent", "child", "factor"))
  expect_no_warning(checkVarNames(summationsData$parent, withunit = FALSE))
  expect_no_warning(checkVarNames(summationsData$child, withunit = FALSE))
  expect_true(all(summationsData$factor == 1))
})
