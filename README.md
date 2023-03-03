
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bnanalysis

<!-- badges: start -->
<!-- badges: end -->

The goal of bnanalysis is to provide a collection of functions for
running BirdNet and analyzing BirdNet results. These functions can be
used to create a semi-automated analysis pipeline.

## Installation

You can install bnanalysis from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AFairbairn/bnanalysis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bnanalysis)
## Install BirdNet
getBN()

## Run BirdNet using default settings
## i is the input files path, as in BirdNet-Analyze
## o is the output path, as in BirdNet Analyze
results <- analyze(i="C:/recordings", o="C:/results")
head(results)
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

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

- Features to add:
  - The ability to accept multiple output formats for combine_results.
  - Function to create output tables for, for example, strange
    detections in Kaleidoscope or other.
  - Function for calculating species frequency - i.e. “minute calls” VAR
    “Voval Activity Rate”
  - Visualization functions for the bird community - see Sethi Norway
    paper for e.g.
