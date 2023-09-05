test_that("removePlus works", {
  expect_identical(removePlus("A|+|B (EJ)"), "A|B (EJ)")
  expect_identical(removePlus("A|+++++|B (EJ)"), "A|B (EJ)")
  expect_identical(removePlus(c("A|+|B (EJ)", "A|+++++|B (EJ)")), c("A|B (EJ)", "A|B (EJ)"))
  expect_identical(removePlus("A|B (EJ)"), "A|B (EJ)")
})
