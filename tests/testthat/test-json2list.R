test_that("json2list works", {
  components <- list(c("A", "B"),
                     list(c("C", "D"), c("E", "F")),
                     list(c("G", "H"), "I"),
                     "J",
                     list(c("K", "L"), c("M", "N", "O", "P"), "Q"))
  componentsstring <- lapply(components, function(x) as.character(jsonlite::toJSON(x)))
  expect_no_warning(result <- json2list(componentsstring))
  expect_true(is.list(result))
  lapply(result, function(x) expect_true(is.list(x)))
  expect_equal(result[[1]], list(components[[1]]))
  expect_equal(result[[2]], components[[2]])
  expect_equal(result[[3]], components[[3]])
  expect_equal(result[[4]], list(components[[4]]))
  expect_equal(result[[5]], components[[5]])
})

test_that("unnestComponents works", {
  template <- data.frame(variable = "FE", components = as.character(jsonlite::toJSON(c("FE|A", "FE|B"))))
  summations <- unnestComponents(template)
  expect_identical(summations, tibble(parent = "FE", child = c("FE|A", "FE|B")))

  template <- data.frame(variable = "FE",
                         components = as.character(jsonlite::toJSON(list(c("FE|A", "FE|B"), c("FE|C", "FE|D")))))
  summations <- unnestComponents(template)
  expect_identical(summations, rbind(tibble(parent = "FE", child = c("FE|A", "FE|B")),
                                     tibble(parent = "FE 1", child = c("FE|C", "FE|D"))))

  template <- data.frame(variable = c("FE", "PE"),
                         components = as.character(jsonlite::toJSON(list("FE|B", c("FE|C", "FE|D")))))
  summations <- unnestComponents(template)
  expect_identical(summations, rbind(tibble(parent = "FE", child = "FE|B"),
                                     tibble(parent = "FE 1", child = c("FE|C", "FE|D")),
                                     tibble(parent = "PE", child = "FE|B"),
                                     tibble(parent = "PE 1", child = c("FE|C", "FE|D"))))
})
