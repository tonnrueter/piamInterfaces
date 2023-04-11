test_that("priceIndices (Add, Fix, IIASA) works", {
  # define simple data structure
  mifdata <- quitte::as.quitte(data.frame(
    "model" = "REMIND",
    "scenario" = c("Base", "Base", "NDC", "NDC"),
    "variable" = "Price|Energy",
    "value" = c(2., 6., 5., 8.),
    "period" = c(2005, 2010, 2005, 2010),
    "region" = "GLO",
    "unit" = "US$2010/GJ"
  ))

  # add structure for expectedPriceIndex, values are assigned later
  expectedPriceIndex <- mifdata
  expectedPriceIndex$variable <- as.factor("Price|Energy|Index")
  expectedPriceIndex$unit <- as.factor("Index (2005 = 1)")

  # write iiasatemplate
  iiasatemplate <- file.path(tempdir(), "iiasatemplate.xlsx")
  templatedata <- data.frame(
    "variable" = c("Price|Energy", "Price|Energy|Index"),
    "unit" = c("US$2010/GJ", "Index (2005 = 1)")
  )
  writexl::write_xlsx(x = templatedata, path = iiasatemplate)

  # calculate price index per scenario individually
  expectedPriceIndex$value <- c(1., 3., 1., 1.6)
  mifdataPI <- priceIndicesAdd(mifdata, "Price|Energy|Index", referenceYear = 2005)
  expect_identical(mifdataPI,
                   rbind(mifdata, expectedPriceIndex))
  mifdataPI <- priceIndicesIIASA(mifdata, iiasatemplate)
  expect_identical(mifdataPI,
                   rbind(mifdata, expectedPriceIndex))

  # calculate price index based on Base scenario
  expectedPriceIndex$value <- c(1., 3., 2.5, 4.)
  mifdataPI <- priceIndicesAdd(mifdata, "Price|Energy|Index", scenBase = "Base", referenceYear = 2005)
  expect_identical(mifdataPI, rbind(mifdata, expectedPriceIndex))
  mifdataPI <- priceIndicesIIASA(mifdata, iiasatemplate, scenBase = "Base")
  expect_identical(mifdataPI, rbind(mifdata, expectedPriceIndex))

  # take data based on 2005, and shift reference year to 2010
  expectedPriceIndex$value <- c(1., 3., 1., 4.)
  mifdataPI <- rbind(mifdata, expectedPriceIndex)
  mifdataPI2010 <- priceIndicesFix(mifdataPI, "Price|Energy|Index", 2010)
  expectedPriceIndex2010 <- expectedPriceIndex
  expectedPriceIndex2010$value <- c(1. / 3, 1., 1. / 4, 1.)
  expectedPriceIndex2010$unit <- as.factor("Index (2010 = 1)")
  expect_identical(mifdataPI2010, rbind(mifdata, expectedPriceIndex2010))

  # correct it back using checkIIASASubmission
  capture.output(mifdataPIIIASA <- checkIIASASubmission(mifdataPI2010, iiasatemplate))
  expect_identical(mifdataPIIIASA, mifdataPI)

})
