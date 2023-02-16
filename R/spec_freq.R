#' Outputs the frequency of each species in a BirdNet results table.
#'
#' This function takes a dataframe in the format of the output of combine_results.R
#' and returns a dataframe of frequency detection rates (VAR). If no overlap is
#' provided, the function assumes that overlap is 0. If no level is provided,
#' the results of the  'table()' function are returned. If an overlap is provided,
#' a window number is assigned to each detection. Detections of the same species
#' that ocurr in the same time period, for example, where BirdNet detected the
#' same call twice due to the overlap, receive the same window number. If no
#' level column is provided, level=F, frequency will be calculated at the level
#' of file. If a level column is provided, for example day, the results will
#' be a frequency per day.
#'
#' @param df a dataframe in the format of combine_results().
#' @param level Defaults to False
#' @return A dataframe of all .csv files in path
#' @export
spec_freq <- function(df, overlap=0, level=F) {
  # Check if overlap is 0 or not (0 is default)
  if(overlap==0){
    if(level==F){
      return(table(df$scientificName))
    } else {

    }
  }
  return()
}
