
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bnanalysis: [BirdNet Analyzer](https://github.com/kahst/BirdNET-Analyzer) in R

<!-- badges: start -->

    #> [1] "[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)"
    #> [1] "[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)"
    #> [1] "[![License: GPL v3.0](https://img.shields.io/badge/license-GPL v3.0-blue.svg)](https://cran.r-project.org/web/licenses/GPL v3.0)"

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

    #> BirdNet is already installed. Checking python environment and dependencies.
    #> /nInstalling pipenv...
    #> Creating virtual environment and installing dependencies...
    #>                                                                      filepath
    #> 1:  D:/acoustic recordings\\AAD1_20210601_040800_Brantstr East 1_351-19-3.wav
    #> 2: D:/acoustic recordings\\AAD1_20210601_041800_Brantstr East 1_1272-31-3.wav
    #> 3: D:/acoustic recordings\\AAD1_20210601_041800_Brantstr East 1_1272-31-3.wav
    #> 4: D:/acoustic recordings\\AAD1_20210601_041800_Brantstr East 1_1273-39-3.wav
    #> 5: D:/acoustic recordings\\AAD1_20210601_041800_Brantstr East 1_1274-21-3.wav
    #> 6: D:/acoustic recordings\\AAD1_20210601_041800_Brantstr East 1_1274-21-3.wav
    #>    start end        scientific_name        common_name confidence lat lon week
    #> 1:     0   3          Turdus merula Eurasian Blackbird     0.1760  -1  -1   -1
    #> 2:     0   3          Turdus merula Eurasian Blackbird     0.2524  -1  -1   -1
    #> 3:     0   3 Phylloscopus collybita  Common Chiffchaff     0.1437  -1  -1   -1
    #> 4:     0   3          Turdus merula Eurasian Blackbird     0.6281  -1  -1   -1
    #> 5:     0   3          Turdus merula Eurasian Blackbird     0.4060  -1  -1   -1
    #> 6:     0   3     Turdus migratorius     American Robin     0.1313  -1  -1   -1
    #>    overlap sensitivity min_conf species_list
    #> 1:       0           1      0.1         None
    #> 2:       0           1      0.1         None
    #> 3:       0           1      0.1         None
    #> 4:       0           1      0.1         None
    #> 5:       0           1      0.1         None
    #> 6:       0           1      0.1         None
    #>                                       model
    #> 1: BirdNET_GLOBAL_3K_V2.2_Model_FP32.tflite
    #> 2: BirdNET_GLOBAL_3K_V2.2_Model_FP32.tflite
    #> 3: BirdNET_GLOBAL_3K_V2.2_Model_FP32.tflite
    #> 4: BirdNET_GLOBAL_3K_V2.2_Model_FP32.tflite
    #> 5: BirdNET_GLOBAL_3K_V2.2_Model_FP32.tflite
    #> 6: BirdNET_GLOBAL_3K_V2.2_Model_FP32.tflite

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
