
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bnanalysis: [BirdNet Analyzer](https://github.com/kahst/BirdNET-Analyzer) in R

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![green](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: GPL
v3.0](https://img.shields.io/badge/license-GPL%20v3.0-blue.svg)](https://cran.r-project.org/web/licenses/GPL%20v3.0)
<!-- badges: end -->

## Description

bnanalysis is both a wrapper around
[BirdNet-Analyzer](https://github.com/kahst/BirdNET-Analyzer) and a tool
for analyzing results derived from BirdNet. It allows users to run
BirdNet directly from R. bnanalysis provides a collection of functions
for installing, running BirdNet and analyzing BirdNet results.
bnanalysis was developed as a tool to create a semi-automated analysis
pipeline.

- Features to-do list:
  - Support for operating systems other than Windows.
  - Function to create output tables for, for example, odd detections in
    Kaleidoscope or other.
  - Function for calculating VAR “Vocal Activity Rate”
  - Visualization functions for the bird community

## About

Developed by Andrew Fairbairn

[Homepage](https://afairbairn.com/) [Chair for Terrestrial
Ecology](https://www3.ls.tum.de/en/toek/home/), Technical University of
Munich

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
