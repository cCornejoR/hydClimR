
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydClimR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

RWeatherTools is a set of functions that allow downloading large data
sets using parallelization, and some functions to perform the processing
of these satellite data, it also includes some functions to generate
inputs for hydrological models.

## Installation

You can install the development version of RWeatherTools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools") 
devtools::install_github("cCornejoR/hydClimR")
library(hydClimR)
```

## Example

This is a basic example which shows you how to download RAIN4PE data:

``` r
## Get the monthly climatology of a stack netcdf and get a gif as well!

shp <- rgdal::readOGR('Data/SHPs/cuenca_mayo.shp')
t <- climatology(years_omit = c(2005,2010),
                grid_path = 'Data/GRIDs/prec_diaria.nc',
                main_title.plot = 'Mayo River basin from PISCOpd',
                shapefile = shp,
                star_date = '1981-01-01',
                end_date = '2016-12-01',
                name_out = 'mayo_basin_climatology',
                variable = 'Precipitation (mm)',
                save_GIF = TRUE,
                return_clip = TRUE,
                save_plot = FALSE)
```

## Overview

This package depends on many other packages, the intention of this
package is to generate quick functions that allow you to do long
processes when writing your code and work with large satellite datasets.
For Peru there is the function of obtaining information directly from
SENAMHI, its operation is still in process. for now you can use the
functions of downloading and preprocessing the data downloaded from the
following satellites: CMORPH, SM2RAIN data sets, CHIRPS, RAIN4PE, PISCO
and TERRACLIMATE.

I will continue to improve the optimization of the funcitons, but for
now it work!
