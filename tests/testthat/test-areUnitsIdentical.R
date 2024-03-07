test_that("areUnitsIdentical works", {
  expect_error(areUnitsIdentical("%"))
  expect_true(areUnitsIdentical("%", "percent"))
  expect_true(areUnitsIdentical("%", "percent", "Percent"))
  expect_true(areUnitsIdentical(c("%", "percent"), "Percent"))
  expect_false(areUnitsIdentical("%", "FE/yr"))
  expect_false(areUnitsIdentical(c("%", "percent"), "FE/yr"))
})
