#' Combines BirdNet results files and adds a column for the .wave file name
#'
#' This takes in a path to the results folder where the BirdNet results .csv
#' files are. This also assumes that you chose the .csv output for BirdNet.
#' It cannot work with other output formats! This function utilizes list.files
#' to get the file list recursive=T is default.
#'
#' @param path The path to the folder containing the .csv files.
#' @param recursive Defaults to True
#' @return A dataframe of all .csv files in path
#' @export
combRes <- function(path, recursive=T) {
  # Get list of files
  files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE,
                        recursive = recursive)
  # End if no files are found and give stop warning
  if (length(files) == 0) {
    stop("No files found in directory")
  }
  # Load all files in to "result"
  result <- data.table::rbindlist(sapply(files, data.table::fread,simplify = FALSE), idcol = "path")
  # Create fileName column
  result[, fileName := sub("\\..*", ".wav", basename(path))]
  # Rename all columns
  data.table::setnames(result, c("path","start","end","scientificName","commonName","confidence","fileName"))
  return(result)
}