
# Tutorial

- To understand how to submit to the IIASA database, read this
  [REMIND tutorial](https://github.com/remindmodel/remind/blob/develop/tutorials/13_Submit_to_IIASA_database.md).
- In the following, we differentiate templates (list of variables and corresponding units used in a project)
  and mappings (specifying which PIAM variable will be mapped to a project variable).

## Mappings

Mappings found in
[the `inst/mappings` folder](https://github.com/pik-piam/piamInterfaces/tree/master/inst/mappings)
serve to map variables from the PIAM framework to variables needed for the submission to databases.
The mappings are `;`-separated files, using `#` as comment character, with the following mandatory columns:

- `variable`: name of the variable in the project template
- `unit`: unit corresponding to `variable`
- `piam_variable`: name of the variable in REMIND / MAgPIE / EDGE-T etc. reporting
- `piam_unit`: unit corresponding to `piam_variable`
- `piam_factor`: factor with which the `piam_variable` has to be multiplied for units to match

Recommended column:
- `description`: description text defining the `variable`. Never use `"` and `;` in the text.
- `source`: abbreviation of the PIAM part where the `piam_variable` comes from.
  Use `B` = Brick, `C` = MAGICC, `M` = MAgPIE, `R` = REMIND, `S` = SDP postprocessing, `T` = EDGE-Transport.
  This column is used to select the variables passed to
  [remind2](https://github.com/pik-piam/remind2/blob/master/tests/testthat/test-convGDX2mif.R#L13-L26)
  and [coupling tests](https://github.com/remindmodel/remind/blob/develop/tests/testthat/test_20-coupled.R).
  If the variable is not normally reported, add a small `x` after the model abbreviation for it to be skipped.

Additionally, some mappings use those columns:
- `idx`: serial number of `variable`
- `Tier`: importance of variable. 1 means most important
- `Comment`: place for comments


To edit a mapping in `R`, use:
```
mappingdata <- getMapping("AR6")
...
write.csv2(mappingdata, "test.csv", na = "", row.names = FALSE, quote = FALSE)
```

Opening the csv files in Excel can be problematic, as it sometimes changes values and quotation marks.
You can edit the files in LibreOffice Calc using these settings in the Text Import dialog:
- Text Import with:
  - Character set: Unicode (UTF-8)
  - Separated by: Semicolon.
- Save with:
  - Character set: Unicode (UTF-8)
  - Field Delimiter: ;
  - String Delimiter: (none)

The github diff on a large semicolon-separated file is often unreadable.
For a human-readable output, save the old version of the mapping and run:
```
remind2::compareScenConf(fileList = c("oldfile.csv", "mappingfile.csv"), row.names = NULL)
```

## Model intercomparison

- To compare the results of different models, pass as `modeldata` a [quitte](https://github.com/pik-piam/quitte/) object or a csv/xlsx file. You get a PDF document for each scenario and each model with area plots for all the summation groups in `AR6` (or `NAVIGATE`) [summation files](https://github.com/pik-piam/piamInterfaces/tree/master/inst/summations) plus line plots for each variable in the `lineplotVariables` vector you supplied. It takes some time, better use a `slurm` job for:
  ```
  plotIntercomparison(modeldata, summationsFile = "AR6", lineplotVariables = c("Temperature|Global Mean", "Population"))
  ```

- If your `modeldata` is not well filtered such that for example model regions are not too different, you can use `interactive = TRUE` which allows to select models, regions, scenarios and variables that you like in your PDF. As `lineplotVariables`, you can also specify mapping names.
  ```
  plotIntercomparison(modeldata, summationsFile = "AR6", lineplotVariables = c("AR6", "AR6_NGFS"), interactive = TRUE)
  ```
