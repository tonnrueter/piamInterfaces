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

for (mapping in names(mappingNames())) {
  test_that(paste0("checkVarNames in mapping ", mapping), {
    mappingData <- getMapping(mapping)
    expect_no_warning(checkVarNames(paste0(mappingData$variable, " (", mappingData$unit, ")")))
    mpiam <- dplyr::filter(mappingData, ! is.na(.data$piam_variable))
    expect_no_warning(checkVarNames(paste0(mpiam$piam_variable, " (", mpiam$piam_unit, ")")))
  })
}
