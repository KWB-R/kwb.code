
[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/github/KWB-R/kwb.code?branch=master&svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-code/branch/master) 
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.code.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.code) 
[![codecov](https://codecov.io/github/KWB-R/kwb.code/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.code) 
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) 
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/kwb.code)]()
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3385170.svg)](https://doi.org/10.5281/zenodo.3385170)

# kwb.code

This package allows you to parse your R scripts and to calculate some staticstics on your code.

## Installation

Install the package from Github:

``` r
#install.packages("remotes")
remotes::install_github("kwb-r/kwb.code")
```

## Basic Usage

If you have a lot of R scripts and you want to know what different packages are loaded from within your scripts, you may use the function `get_names_of_used_packages`:

``` r
# Set path to directory in which to look recursively for R scripts
root_dir <- "~/Desktop/R-Development"

# Get the names of used packages  
packages <- kwb.code::get_names_of_used_packages(root_dir)

# Show the names of the packages
packages
```

## Documentation

Release: <https://kwb-r.github.io/kwb.code>

Development: <https://kwb-r.github.io/kwb.code/dev>
