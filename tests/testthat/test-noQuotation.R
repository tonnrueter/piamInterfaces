for (t in names(mappingNames())) {
  test_that(paste("No double quotation mark used in mapping", t), {
    code <- grep('"', readLines(mappingNames(t)), value = TRUE)
    if (length(code) > 0) {
      warning(paste0("These lines in mapping ", t, " contain double quotes. They create a mess, avoid them.\n"),
              paste(code, collapse = "\n"))
    }
    expect_true(length(code) == 0)
  })
}
