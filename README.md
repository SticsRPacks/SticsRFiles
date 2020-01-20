
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SticsRFiles

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Travis build
status](https://travis-ci.org/SticsRPacks/SticsRFiles.svg?branch=master)](https://travis-ci.org/SticsRPacks/SticsRFiles)
[![Codecov test
coverage](https://codecov.io/gh/SticsRPacks/SticsRFiles/branch/master/graph/badge.svg)](https://codecov.io/gh/SticsRPacks/SticsRFiles?branch=master)
<!-- badges: end -->

The goal of SticsRFiles is to perform manipulations of the Stics model
files either on XML files (used by the JavaStics GUI) or on text files
used by the model fortran executable.

The basic functionnalites allows to read parameters names and values
through XML queries and replace parameters values in files.

Advanced functionnalities are dedicated to produce XML parameters files
using a mailing process like from XML templates and Excel sheets
containing multiple simulations contexts. A JavaStics workspace is
generated and directly usable from the JavaStics interface (GUI or
command line), or with an R JavaStics interface provided by the
[SticsOnR](https://SticsRPacks.github.io/SticsOnR) package.

## Pre-requisities and technical tips

Some information about software requirements and operating system
constraints are given in the SticsOnR package documentation
[here](https://sticsrpacks.github.io/SticsOnR).

## Installation

The development version from [GitHub](https://github.com/) can be
installed with:

``` r
devtools::install_github("SticsRPacks/SticsRFiles@*release")
```

Or using the lightweight
[remotes](https://github.com/r-lib/remotes#readme) package:

``` r
# install.packages("remotes")
remotes::install_github("SticsRPacks/SticsRFiles@*release")
```

## Examples

### Files manipulations

  - A description of how to use the functions for manipulating XML input
    files is detailed
    [here](https://sticsrpacks.github.io/SticsRFiles/articles/Manipulating_Stics_XML_files.html)

  - A description of how to use the functions for manipulating text
    input files will be available soon.

### Files generation

  - A description of mailing functions usefull for generating XML input
    files from usms parameters data stored in Excel files sheets will be
    available soon.

  - A description of functions for generating text input files from XML
    one of usms parameters is available
    [here](https://sticsrpacks.github.io/SticsRFiles/articles/Generating_Stics_text_files.html).

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## Authors and acknowledgments

The SticsRFiles package is developed by Patrice Lecharpentier, Rémi Vezy
and the [SticsRFiles
Team](https://github.com/orgs/SticsRPacks/teams/sticsrfiles).
