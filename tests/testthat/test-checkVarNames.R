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
  w <- capture_warnings(checkVarNames(qeAR6))
  expect_length(w, 0)
})

for (mapping in names(mappingNames())) {
  mappingData <- getMapping(mapping)
  test_that(paste0("checkVarNames for variable in mapping ", mapping), {
    expect_no_warning(checkVarNames(unitjoin(mappingData$variable, mappingData$unit)))
  })
  test_that(paste0("checkVarNames for piam_variable in mapping ", mapping), {
    mpiam <- dplyr::filter(mappingData, ! is.na(.data$piam_variable))
    expect_no_warning(checkVarNames(unitjoin(mpiam$piam_variable, mpiam$piam_unit)))
  })
}
