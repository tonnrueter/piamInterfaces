
## Tutorials

To understand how to submit to the IIASA database, read this [REMIND tutorial](https://github.com/remindmodel/remind/blob/develop/tutorials/13_Submit_to_IIASA_database.md).

To edit a template in `R`, use:
```
templatedata <- getTemplate("AR6")
...
write.csv2(templatedata, "test.csv", na = "", row.names = FALSE, quote = FALSE)
```

The github diff on a large semicolon-separated file is often unreadable.
For a human-readable output, save the old version of the mapping and run:
```
remind2::compareScenConf(fileList = c("oldfile.csv", "mappingfile.csv"), row.names = NULL)
```

To compare the results of different models as saved in `modeldata` ([quitte](https://github.com/pik-piam/quitte/) object or csv/xlsx file) that is appropriately filtered such that model regions are not too different, run:
```
piamInterfaces::plotIntercomparison(modeldata, summationsFile = "AR6", lineplotVariables = c("Temperature|Global Mean", "Population"))
```
and you get a PDF document for each scenario and each model with area plots for all the summation groups in `AR6` (or `NAVIGATE`) [summation files](https://github.com/pik-piam/piamInterfaces/tree/master/inst/summations) plus line plots for each variable in the vector you supplied. It takes some time, better use a `slurm` job.
