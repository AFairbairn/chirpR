#' analyze calls the BirdNet analayze function and returns the results or nothing
#'
#' This takes in a path to the results folder where the BirdNet results .csv
#' files are. This also assumes that you chose the .csv output for BirdNet.
#' It cannot work with other output formats! This function utilizes list.files
#' to get the file list recursive=T is default.
#'
#' @param i Path to input .wav files for BirdNet analysis.
#' @param o Output file location
#' @param ... Other parameters to be passed to analyze.py
#' @param result Logical, determines if you want to import the result as a dataframe defaults True
#' @return Nothing or a CSV file with the species detection results
#' @export
analyze <- function(i, o, rtype=csv, ..., result=TRUE){


}
