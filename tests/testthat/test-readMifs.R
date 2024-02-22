test_that("readMifs works", {
  filename <- "./data/testdata.mif"
  d <- testdata
  expect_identical(d, readMifs(d))
  expect_identical(d, readMifs(filename))
  expect_identical(d, readMifs(dirname(filename)))
  expect_identical(rbind(d, d), readMifs(filename, dirname(filename)))
})
