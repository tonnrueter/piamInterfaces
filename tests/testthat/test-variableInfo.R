test_that("test variableInfo", {
  expect_message(variableInfo("GDP|PPP"), "Results from template")
  expect_message(variableInfo("Emi|CO2"), "Export variable groups found in")
  expect_message(variableInfo("Quark|mit Soße"), "Nothing found")
  expect_message(variableInfo("FE|++|CDR"), "Results from template")
  for (template in c("AR6", "NAVIGATE")) {
    expect_message(variableInfo("GDP|PPP", template = template), "Results from template")
    expect_message(variableInfo("Quark|mit Soße", template = template), "Nothing found")
  }
})
