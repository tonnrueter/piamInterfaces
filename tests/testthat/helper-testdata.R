testdata <- quitte::as.quitte("data/testdata.mif")
qeAR6 <- quitte::quitte_example_dataAR6 %>%
  quitte::as.quitte(na.rm = TRUE) %>%
  filter(! .data$model %in% "GCAM") %>%
  quitteSort() %>%
  droplevels()
