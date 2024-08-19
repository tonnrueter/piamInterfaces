test_that("sumNamesWithFactors works", {
  mappingData <- data.frame(
    variable = c("A", "A", "B", "B", "B", "C", "D", "E"),
    piam_variable = c("M", "N", "M", "N", "O", "M", "P", NA),
    piam_factor = c(1, NA, "-1", "-2", 1, "-2", "", NA)
  )
  expect_equal(sumNamesWithFactors(mappingData, "A"),
               "+ M + N")
  expect_equal(sumNamesWithFactors(mappingData, "B"),
               "- M -2 N + O")
  expect_equal(sumNamesWithFactors(mappingData, "C"),
               "-2 M")
  expect_equal(sumNamesWithFactors(mappingData, "D"),
               "+ P")
  expect_equal(sumNamesWithFactors(mappingData, "E"),
               "NA")
})
