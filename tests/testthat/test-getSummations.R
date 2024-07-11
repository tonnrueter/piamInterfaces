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
  }
})
