#' analyze calls the BirdNet analayze function and returns the results or nothing
#'
#' This takes in a path to the results folder where the BirdNet results .csv
#' files are. This also assumes that you chose the .csv output for BirdNet.
#' It cannot work with other output formats! This function utilizes list.files
#' to get the file list recursive=T is default.
#'
#' @param i Path to input .wav files for BirdNet analysis.
#' @param o Output file location
#' @param rtype The output file type. Defaults to r
#' @param ... Other parameters to be passed to analyze.py
#' @param result Logical, determines if you want to import the result as a dataframe defaults True
#' @param path If specifying the location of BirdNet. If installed with this package, leave blank.
#' @return Nothing or a CSV file with the species detection results
#' @export
analyze <- function(i, o, rtype="r", ..., result=TRUE, path){
  # Stop message for missing i
  if(missing(i)){
    stop("The argument i is missing. You must set an input directory!")
  }

  # Create output directory if none provided
  if(missing(o)){
    o = file.path(getwd(), "bnResults")
    if(!dir.exists(o)){
      dir.create(o)
    }
  }

  # Use default BirdNet path if none provided
  if(missing(path)) {
    path = file.path(path.expand("~"), "BirdNET-Analyzer/BirdNET-Analyzer-main")
  }

  # Check if rtype is valid
  if(!rtype %in% c("table", "audacity", "r", "csv")) {
    stop("Invalid output type. Must be table, audacity, r or csv. Default is r.")
  }

  # Check that input directory contains .wav files
  if (!any(grepl("\\.(wav|flac|mp3|ogg|m4a)$", list.files(i, recursive=T)))) {
    stop("No audio files found in the input directory.")
  }

  # Prepare list of args to pass to BirdNet
  args = c(list(i=i, o=o, rtype=rtype), list(...))
  argsStr <- paste0("--", names(args), " ",
                    ifelse(names(args) %in% c("i", "o"),
                           paste0("\"", unlist(args), "\""),
                           unlist(args)), collapse = " ")

  # Change wd to call BirdNet
  oldWd = getwd()
  setwd(path)

  # Call BirdNet
  system(paste("pipenv run python", "analyze.py", argsStr))

  # Return working directory
  setwd(oldWd)

  if(result) {
    return(combRes(o, rtype, recursive = T))
  }

}
