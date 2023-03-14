#' Combines BirdNet results files
#'
#' BirdNet creates a results file for each audio file it analyzes. This takes in
#' a path to the results folder where the BirdNet results files are located and
#' combines them in to one dataframe for working with in R or saving for analysis
#' in other software. This function utilizes list.files to get the file list
#' recursive=T is default.
#'
#' @param path The path to the folder containing the .csv files.
#' @param recursive Defaults to True
#' @param rtype The BirdNet output file type. Defaults to r.
#' @return A dataframe of all files in path
#' @export
#' @examples
#' \dontrun{
#' results <- combRes(path="C:/results folder/", rtype="table")}
combRes <- function(path, rtype="r", recursive=T) {
  # Check if rtype is valid and set seperator
  if(rtype %in% c("table", "audacity")) {
    sep = "\t"
    # Get list of files
    files = list.files(path = path, pattern = "\\.txt$", full.names = TRUE,
                       recursive = recursive)
  } else if(rtype %in% c("r", "kaleidoscope", "csv")) {
    sep = ","
    # Get list of files
    files = list.files(path = path, pattern = "\\.csv$", full.names = TRUE,
                       recursive = recursive)
  } else {
    stop("Invalid output type. Must be table, audacity, r or csv. Default is r.")
  }

  # End if no files are found and give stop warning
  if (length(files) == 0) {
    stop("No files found in directory")
  }

  # Load all files in to "result"
  result = data.table::rbindlist(sapply(files, data.table::fread, simplify = FALSE, sep = sep))


  return(result)
}
