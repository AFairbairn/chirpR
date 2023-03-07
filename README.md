
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bnanalysis: [BirdNet Analyzer](https://github.com/kahst/BirdNET-Analyzer) in R

<!-- badges: start -->

badger::badge_lifecycle(“experimental”, “red”)
badger::badge_repostatus(“Active”, “green”) badger::badge_license(“GPL
v3.0”)

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

- Features to-do list:
  - Support for operating systems other than Windows.
  - Function to create output tables for, for example, odd detections in
    Kaleidoscope or other.
  - Function for calculating species frequency - i.e. “minute calls” VAR
    “Vocal Activity Rate”
  - Visualization functions for the bird community - see Sethi Norway
    paper for e.g.
