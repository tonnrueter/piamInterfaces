test_that("checkVarNames works", {
  fine <- c("PE (EJ)", "PE (NA/yr)", "FE|What (yr)", "FE|Electricity (EJ/yr)")
  for (v in fine) {
    expect_no_warning(checkVarNames(v))
  }
  wrong <- c("PE", "PE| (EJ)", "PE||Elec (EJ)", "PE||Elec", "PE  (EJ)", "PE (NA)",
             "NA|PE (EJ)", "PE|NA|What (EJ)", " PE (EJ)", "PE (EJ) ")
  for (v in wrong) {
    w <- capture_warnings(checkVarNames(v))
    expect_true(length(w) > 0)
  }
})

for (template in names(templateNames())) {
  test_that(paste0("checkVarNames in template ", template), {
    t <- getTemplate(template)
    expect_no_warning(checkVarNames(paste0(t$variable, " (", t$unit, ")")))
    tpiam <- dplyr::filter(t, ! is.na(.data$piam_variable), .data$piam_variable != "TODO")
    expect_no_warning(checkVarNames(paste0(tpiam$piam_variable, " (", tpiam$piam_unit, ")")))
  })
}
