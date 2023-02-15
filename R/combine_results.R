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
combine_results <- function(path, recursive=T) {
  files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE,
                        recursive = recursive)
  if (length(files) == 0) {
    stop("No files found in directory")
  }
  #names(files) <- sub("\\..*", ".wav", basename(files))
  result <- data.table::rbindlist(sapply(files, data.table::fread,simplify = FALSE), idcol = "path")
  result$fileName <- with(result, sub("\\..*", ".wav", basename(path)))
  colnames(result) <- c("path","start","end","scientificName","commonName","confidence","fileName")
  return(result)
}
