
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chirpR: [BirdNet-Analyzer](https://github.com/kahst/BirdNET-Analyzer) in R

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R build
status](https://github.com/Afairbairn/chirpR/workflows/R-CMD-check/badge.svg)](https://github.com/Afairbairn/chirpR/actions)
[![green](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: CC BY-NC-SA
4.0](https://img.shields.io/badge/license-CC%20BY--NC--SA%204.0-blue.svg)](https://cran.r-project.org/web/licenses/CC%20BY-NC-SA%204.0)
<!-- badges: end -->

## Description

`chirpR` is an R package that simplifies the process of analyzing and
interpreting BirdNet data. It provides a user-friendly wrapper for
[BirdNet-Analyzer](https://github.com/kahst/BirdNET-Analyzer), allowing
users to easily install and run BirdNet directly from R. Additionally,
`chirpR` includes a collection of functions for analyzing and
visualizing BirdNet output. Developed with a semi-automated analysis
pipeline in mind, `chirpR` streamlines the BirdNet analysis process.

-   Features to-do list:
    -   Support for operating systems other than Windows.
    -   Function to create output tables for, for example, odd
        detections in Kaleidoscope or other.
    -   Function for calculating VAR “Vocal Activity Rate”
    -   Visualization functions for the bird community

## About

Developed by 🦜Andrew Fairbairn

[Chair for Terrestrial Ecology](https://www3.ls.tum.de/en/toek/home/),
Technical University of Munich

[Homepage](https://afairbairn.com/)

## Installation

You can install chirpR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AFairbairn/chirpR")
```

## Example

This is a basic example which shows you how to install and use BirdNet:

``` r
library(chirpR)

setwd("E:/myProject")

## Install BirdNet
getBN()
#> BirdNet is already installed. Checking python environment and dependencies.
#> /nInstalling pipenv...
#> Creating virtual environment and installing dependencies...

## Run BirdNet using default settings
## i is the input files path, as in BirdNet-Analyze
results <- analyze(i="E:/acoustic recordings")
head(results[,c(4:6)])
#>       scientific_name        common_name confidence
#> 1:             Engine             Engine     0.1579
#> 2:      Turdus merula Eurasian Blackbird     0.1027
#> 3: Scolopax rusticola  Eurasian Woodcock     0.1033
#> 4:      Turdus merula Eurasian Blackbird     0.3508
#> 5:      Turdus merula Eurasian Blackbird     0.5635
#> 6:  Turdus viscivorus      Mistle Thrush     0.2606
```
