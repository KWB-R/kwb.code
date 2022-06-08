[![R-CMD-check](https://github.com/KWB-R/kwb.code/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.code/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.code/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.code/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.code/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.code)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.code)]()
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3385170.svg)](https://doi.org/10.5281/zenodo.3385170)
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.code)](https://kwb-r.r-universe.dev/)

This package allows you to parse your R scripts and to calculate some staticstics on your code.

## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
  
# Download and install kwb.code in R
install.packages('kwb.code')

# Browse the kwb.code manual pages
help(package = 'kwb.code')
```

## Basic Usage

If you have a lot of R scripts and you want to know what different packages
are loaded from within your scripts, you may use the function
`get_names_of_used_packages`:

```r
# Set path to directory in which to look recursively for R scripts
root_dir <- "~/Desktop/R-Development"

# Get the names of used packages  
packages <- kwb.code::get_names_of_used_packages(root_dir)

# Show the names of the packages
packages
```
