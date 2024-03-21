test_that("test variableInfo", {
  expect_message(variableInfo("GDP|PPP"), "Results from mapping")
  expect_message(variableInfo("Emi|CO2"), "Export variable groups found in")
  expect_message(variableInfo("Quark|mit Soße"), "Nothing found")
  expect_message(variableInfo("FE|++|CDR"), "Results from mapping")
  for (mapping in c("AR6", "NAVIGATE")) {
    expect_message(variableInfo("GDP|PPP", mapping = mapping), "Results from mapping")
    expect_message(variableInfo("Quark|mit Soße", mapping = mapping), "Nothing found")
  }
})
