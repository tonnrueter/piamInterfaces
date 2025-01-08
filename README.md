# Project specific interfaces to REMIND / MAgPIE

R package **piamInterfaces**, version **0.40.7**

[![CRAN status](https://www.r-pkg.org/badges/version/piamInterfaces)](https://cran.r-project.org/package=piamInterfaces) [![R build status](https://github.com/pik-piam/piamInterfaces/workflows/check/badge.svg)](https://github.com/pik-piam/piamInterfaces/actions) [![codecov](https://codecov.io/gh/pik-piam/piamInterfaces/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/piamInterfaces) [![r-universe](https://pik-piam.r-universe.dev/badges/piamInterfaces)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Project specific interfaces to REMIND / MAgPIE.

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
- `unit`: unit corresponding to `variable`. If the IIASA template has no unit, use `unitless`.
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
- `interpolation`: sets the interpolation method for the `variable` (i.e. not `piam_variable`) (currently only supports `linear`). When set to `linear`, adds yearly values between 2005 and 2100 through linear interpolation for the selected output variables.

Additionally, some mappings use those columns:
- `idx`: serial number of `variable`
- `Tier`: importance of variable. 1 means most important
- `Comment`: place for comments
- `weight`: calculates a weighted average of multiple entries of `piam_variable`. Provide a different `piam_variable` in this column, and `generateIIASASubmission()` will split the data on the rows which contain weight pointers, resolve these weights into numerical values (via a join operation between the submission and the input data) and then modify the value based on the weighting. This takes place in the private .resolveWeights method.

To edit a mapping in `R`, use:
```
mappingdata <- getMapping("AR6")
...
write.table(mappingdata, "inst/mappings/test_mapping.csv", na = "",  dec = ".", sep = ";", row.names = FALSE, quote = FALSE)
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
remind2::compareScenConf(fileList = c("oldfile.csv", "mappingfile.csv"), row.names = NULL, expanddata = FALSE)
```
On the PIK cluster, you can run `comparescenconf mapping_AR6.csv` in the `inst/mappings` folder and it will compare to a recent `master` version.

### Creating a new mapping

Since templates contain between several hundreds and a few thousand variables, relying on existing mappings can save substantial amounts of work compared to setting up a new mapping from scratch. Since the template itself is most likely built based on earlier templates from other projects, chances are good that existing mappings already provide parts of the required new mapping. Using `R`, we describe a simple way to create a new mapping `mapping_NEW.csv` based on existing mappings. 

1. Identify which existing mappings are most relevant for your new mapping. Criteria might include the time at which the existing mapping was created and the proximity of the templates (e.g. follow-up project). If you are unsure, ask your experienced colleagues for advice. This provides you with a list of existing mappings that is ordered by relevance, say `mapping_OLD1.csv`, ... , `mapping_OLD9.csv`. 
2. Use `read.csv2` to get the template as a dataframe `template`.
3. Looping over the existing mappings in descending order of relevance,
    - use `getMapping` to get the existing mapping `mapping_OLDi` as a dataframe,
    - `filter` `mapping_OLDi` for variables which are contained in `template` and which have not yet been added to `mapping_NEW`,
    - `left_join` (by variable) the filtered `mapping_OLDi` with `template`  to add the information from the template (consider using `str_to_lower` for case-insensitive matching when filtering and joining),
    - `select`/`mutate` the columns of the joined dataframe to keep the desired columns for the new mapping (see above for description of mandatory and recommended columns),
    - `bind_rows` to `mapping_NEW`
5. Use `write.csv2` to export `mapping_NEW` as `mapping_NEW.csv`. It is recommended to make a few checks (e.g. by looking at all variables for which the description or the unit does not agree between the existing mapping and the template).

## Model intercomparison

- To compare the results of different models, pass as `modeldata` a [quitte](https://github.com/pik-piam/quitte/) object or a csv/xlsx file. You get a PDF document for each scenario and each model with area plots for all the summation groups in `AR6` (or `NAVIGATE`) [summation files](https://github.com/pik-piam/piamInterfaces/tree/master/inst/summations) plus line plots for each variable in the `lineplotVariables` vector you supplied. It takes some time, better use a `slurm` job for:
  ```
  plotIntercomparison(modeldata, summationsFile = "AR6", lineplotVariables = c("Temperature|Global Mean", "Population"))
  ```

- If your `modeldata` is not well filtered such that for example model regions are not too different, you can use `interactive = TRUE` which allows to select models, regions, scenarios and variables that you like in your PDF. As `lineplotVariables`, you can also specify mapping names.
  ```
  plotIntercomparison(modeldata, summationsFile = "AR6", lineplotVariables = c("AR6", "AR6_NGFS"), interactive = TRUE)
  ```

## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("piamInterfaces")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Falk Benke <benke@pik-potsdam.de>.

## Citation

To cite package **piamInterfaces** in publications use:

Benke F, Richters O (2025). "piamInterfaces: Project specific interfaces to REMIND / MAgPIE." Version: 0.40.7, <https://github.com/pik-piam/piamInterfaces>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {piamInterfaces: Project specific interfaces to REMIND / MAgPIE},
  author = {Falk Benke and Oliver Richters},
  date = {2025-01-08},
  year = {2025},
  url = {https://github.com/pik-piam/piamInterfaces},
  note = {Version: 0.40.7},
}
```
