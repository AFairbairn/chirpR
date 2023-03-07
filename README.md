
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bnanalysis: [BirdNet Analyzer](https://github.com/kahst/BirdNET-Analyzer) in R

<!-- badges: start -->

``` r
badger::badge_lifecycle("experimental", "red")
#> [1] "[![](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)"
badger::badge_repostatus("Active", "green")
#> [1] "[![green](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)"
badger::badge_license("GPL v3.0")
#> [1] "[![License: GPL v3.0](https://img.shields.io/badge/license-GPL v3.0-blue.svg)](https://cran.r-project.org/web/licenses/GPL v3.0)"
```

<!-- badges: end -->

## Author

Andrew Fairbairn <https://afairbairn.com/>

bnanalysis is to provides a collection of functions for installing,
running BirdNet and analyzing BirdNet results. These functions can be
used to create a semi-automated analysis pipeline.

## Installation

You can install bnanalysis from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AFairbairn/bnanalysis")
```

## Example

This is a basic example which shows you how to install and use BirdNet:

``` r
library(bnanalysis)

setwd("D:/myProject")

## Install BirdNet
getBN()
#> BirdNet is already installed. Checking python environment and dependencies.
#> /nInstalling pipenv...
#> Creating virtual environment and installing dependencies...

## Run BirdNet using default settings
## i is the input files path, as in BirdNet-Analyze
results <- analyze(i="D:/acoustic recordings")
head(results[,c(4:6)])
#>           scientific_name        common_name confidence
#> 1:          Turdus merula Eurasian Blackbird     0.1760
#> 2:          Turdus merula Eurasian Blackbird     0.2524
#> 3: Phylloscopus collybita  Common Chiffchaff     0.1437
#> 4:          Turdus merula Eurasian Blackbird     0.6281
#> 5:          Turdus merula Eurasian Blackbird     0.4060
#> 6:     Turdus migratorius     American Robin     0.1313
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

- Features to-do list:
  - Support for operating systems other than Windows.
  - Function to create output tables for, for example, odd detections in
    Kaleidoscope or other.
  - Function for calculating species frequency - i.e. “minute calls” VAR
    “Vocal Activity Rate”
  - Visualization functions for the bird community - see Sethi Norway
    paper for e.g.
