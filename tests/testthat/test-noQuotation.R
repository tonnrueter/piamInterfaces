for (t in names(templateNames())) {
  test_that(paste("No double quotation mark used in template", t), {
    code <- grep('"', readLines(templateNames(t)), value = TRUE)
    if (length(code) > 0) {
      warning(paste0("These lines in template ", t, " contain double quotes. They create a mess, avoid them.\n"),
              paste(code, collapse = "\n"))
    }
    expect_true(length(code) == 0)
  })
}
