#' Outputs the frequency of each species in a BirdNet results table.
#'
#' This function takes a dataframe in the format of the output of combine_results.R
#' with additional columns for time and date. It returns a dataframe of vocal
#' activity rates (VAR). is calculated by dividing the number of vocalizations
#' for each species by the total duration of the recording in minutes or the
#' number of detections per minute of recording. Date is assumed to be of class
#' date in the iso ISO 8601 format of YYYY-mm-dd. Time is assumed to be in the format HHMMSS. Date is
#' assumed to be If no overlap is provided, the function assumes that
#' overlap is 0.
#'
#' @param df A dataframe in the format of combine_results().
#' @param overlap The value of overlap used in the BirdNet analysis
#' @param site Logical, if a site column is included or not defaults False
#' @param length Total number of minutes of recording per day
#' @return A dataframe of all .csv files in path
#' @export
#' @importFrom stats var
vari <- function(df, overlap=0, site=F, length) {
  # Check if overlap is 0 or not (0 is default)
  if(overlap==0){
    if(site==F){
      varTab <- data.table::setDT(df)[, .(var = .N/length), by = .(date, scientificName)][
        , .(var = mean(var)), by = .(date, scientificName)]
    } else {
      varTab <- data.table::setDT(df)[, .(var = .N/length), by = .(site, date, scientificName)][
        , .(var = mean(var)), by = .(site, date, scientificName)]
    }
    return(varTab)
  }
  return()
}
