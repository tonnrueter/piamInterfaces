test_that("basic checks on summation Groups", {
  for (template in names(summationsNames())) {
    expect_silent(summationsData <- getSummations(template))
    expect_true(all(c("parent", "child", "factor") %in% names(summationsData)))
    expect_true(class(summationsData) == "data.frame")
  }
})
