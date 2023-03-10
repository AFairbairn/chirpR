
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chirpR: tools for automated acoustic analyses in R

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
interpreting bird acoustic monitoring data. It provides a user-friendly
wrapper for
[BirdNet-Analyzer](https://github.com/kahst/BirdNET-Analyzer) and
[ecoVAD](https://github.com/NINAnor/ecoVAD), allowing users to easily
install and run BirdNet and ecoVAD directly from R. Additionally,
`chirpR` includes a collection of functions for analyzing and
visualizing BirdNet output. Developed with a semi-automated analysis
pipeline in mind, `chirpR` streamlines the analysis process.

## About

Developed by 🦜Andrew Fairbairn

Current lab: [Chair for Terrestrial
Ecology](https://www3.ls.tum.de/en/toek/home/), Technical University of
Munich

[Homepage](https://afairbairn.com/)

## Installation

You can install chirpR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AFairbairn/chirpR")
```

## Example

Installing and running BirdNet:

``` r
library(chirpR)

## Install BirdNet
birdNet.install()
#> Python Python 3.11.2 is installed.
#> Python Python 3.10.10 is installed.
#> Using Python version: Python 3.10.10
#> Getting BirdNet...
#> Download completed with exit code: 0
#> Creating virtual environment...
#> Done! BirdNET is ready to use.

## Run BirdNet using default settings
## i is the input files path, as in BirdNet-Analyze
results <- birdNet.analyze(i="E:/acoustic recordings", o="E:/bnResults")
head(results[,c(4:6)])
#>       scientific_name        common_name confidence
#> 1:             Engine             Engine     0.1579
#> 2:      Turdus merula Eurasian Blackbird     0.1027
#> 3: Scolopax rusticola  Eurasian Woodcock     0.1033
#> 4:      Turdus merula Eurasian Blackbird     0.3508
#> 5:      Turdus merula Eurasian Blackbird     0.5635
#> 6:  Turdus viscivorus      Mistle Thrush     0.2606
```

## Example

Setup ecoVAD:

``` r
library(chirpR)

## Setup ecoVad
#ecoVAD.setup()
```
