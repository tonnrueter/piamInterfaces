# Project specific interfaces to REMIND / MAgPIE

R package **piamInterfaces**, version **0.0.16**

[![CRAN status](https://www.r-pkg.org/badges/version/piamInterfaces)](https://cran.r-project.org/package=piamInterfaces)  [![R build status](https://github.com/pik-piam/piam_interfaces/workflows/check/badge.svg)](https://github.com/pik-piam/piam_interfaces/actions) [![codecov](https://codecov.io/gh/pik-piam/piam_interfaces/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/piam_interfaces) [![r-universe](https://pik-piam.r-universe.dev/badges/piamInterfaces)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

Project specific interfaces to REMIND / MAgPIE.


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

Benke F, Richters O (2022). _piamInterfaces: Project specific interfaces to REMIND / MAgPIE_. R package version 0.0.16, <https://github.com/pik-piam/piam_interfaces>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {piamInterfaces: Project specific interfaces to REMIND / MAgPIE},
  author = {Falk Benke and Oliver Richters},
  year = {2022},
  note = {R package version 0.0.16},
  url = {https://github.com/pik-piam/piam_interfaces},
}
```
