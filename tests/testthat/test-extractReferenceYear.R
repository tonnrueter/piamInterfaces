test_that("check extractReferenceYear", {
  expect_identical("1", extractReferenceYear("Index (1 = 1)"))
  expect_identical("2020", extractReferenceYear("Index (2020 = 1)"))
  expect_identical("20212", extractReferenceYear("Index (20212 = 1)"))
  expect_identical(c("1", "2"), extractReferenceYear(c("Index (1 = 1)", "Index (2 = 1)")))
  expect_error(extractReferenceYear("Index (2020a = 1)"))
  expect_error(extractReferenceYear("index (2020 = 1)"))
  expect_error(extractReferenceYear(c("Index (2020 = 1)", "whatever")))
})
