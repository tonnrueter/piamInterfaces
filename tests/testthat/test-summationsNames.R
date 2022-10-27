test_that("check that main summation groups are found", {
  expect_true(all(c("AR6") %in% names(summationsNames())))
  expect_true(grepl("summation_groups_AR6.csv$", summationsNames("AR6")))
  expect_true(file.exists(summationsNames("AR6")))
})
