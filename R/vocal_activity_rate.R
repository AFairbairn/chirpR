#' Calculate vocal activity rates (VAR) from bird detection data
#'
#' This function calculates vocal activity rates using interval deduplication.
#' VAR represents the rate of vocal detections per unit time, calculated per species.
#' Results are returned by species and site (if site_col is provided).
#'
#' @param df A dataframe containing bird detection data.
#' @param interval_unit Character string for time interval grouping. Can be any value accepted by \code{lubridate::floor_date()}, such as "second", "minute", "hour", "day", "week", "month", "quarter", "year", or durations like "15 minutes", "30 seconds", etc. Default is "minute".
#' @param time_col Character string specifying the column name containing detection time. This should be the actual time when the detection occurred (not start time + offset). Default is "timestamp".
#' @param date_col Character string specifying the date column name (if separate from time_col). Default is "date".
#' @param species_col Character string specifying the species column name. Default is "scientific_name".
#' @param site_col Character string specifying the site column name. Default is "site". If this column exists in the data, results will include site information.
#' @param recording_length_col Character string specifying column with total recording days (optional). Default is "recording_length".
#' @param time_format Character string specifying time format if time_col needs parsing. Common formats: "%H:%M:%S", "%H%M%S", "%Y-%m-%d %H:%M:%S". Default is NULL (auto-detect).
#' @return A dataframe with VAR results. Columns will include the species column, site column (if provided), number of detections, days recorded, and calculated VAR.
#'   Results are ordered by species name.
#' @export
#' @importFrom data.table setDT copy uniqueN first
#' @importFrom lubridate ymd floor_date
#' @examples
#' \dontrun{
#' # Basic usage - species-level VAR with default parameters
#' var_result <- vocal_activity_rate(df)
#'
#' # With 15-minute intervals and custom column names
#' var_result <- vocal_activity_rate(df,
#'                             interval_unit = "15 minutes",
#'                             time_col = "detection_time",
#'                             species_col = "scientific_name",
#'                             site_col = "site",
#'                             recording_length_col = "days_recorded")
#'
#' # Daily intervals
#' var_result <- vocal_activity_rate(df,
#'                             interval_unit = "day",
#'                             time_col = "detection_datetime",
#'                             species_col = "species",
#'                             date_col = NULL)  # No separate date column
#'
#' # Using other interval units supported by lubridate::floor_date
#' var_result <- vocal_activity_rate(df, interval_unit = "week")
#' var_result <- vocal_activity_rate(df, interval_unit = "30 seconds")
#' var_result <- vocal_activity_rate(df, interval_unit = "2 hours")
#' }
vocal_activity_rate <- function(df,
                                interval_unit = "minute",
                                time_col = "timestamp",
                                date_col = "date",
                                species_col = "scientific_name",
                                site_col = "site",
                                recording_length_col = "recording_length",
                                time_format = NULL) {

  #--------------------------------------------------------------------------- #
  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data.frame")
  }

  # Check required columns exist
  required_cols <- c(species_col, time_col)
  if (!is.null(site_col)) required_cols <- c(required_cols, site_col)

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  #--------------------------------------------------------------------------- #
  # Work with a copy to avoid modifying original data
  dt <- data.table::setDT(data.table::copy(df))

  # Handle datetime creation based on available columns
  if (inherits(dt[[time_col]], c("POSIXct", "POSIXt"))) {
    # Already datetime
    dt$detection_time <- dt[[time_col]]
  } else if (date_col %in% names(dt)) {
    # Separate date and time columns
    if (!inherits(dt[[date_col]], "Date")) {
      dt[[date_col]] <- lubridate::ymd(dt[[date_col]])
    }

    # Parse time based on format
    if (is.character(dt[[time_col]])) {
      if (is.null(time_format)) {
        # Auto-detect common formats
        if (any(grepl(":", dt[[time_col]]))) {
          dt$detection_time <- as.POSIXct(paste(dt[[date_col]], dt[[time_col]]),
                                          format = "%Y-%m-%d %H:%M:%S")
        } else {
          # Assume HHMMSS format
          dt[[time_col]] <- sprintf("%06s", dt[[time_col]])
          time_formatted <- paste0(substr(dt[[time_col]], 1, 2), ":",
                                   substr(dt[[time_col]], 3, 4), ":",
                                   substr(dt[[time_col]], 5, 6))
          dt$detection_time <- as.POSIXct(paste(dt[[date_col]], time_formatted))
        }
      } else {
        dt$detection_time <- as.POSIXct(paste(dt[[date_col]], dt[[time_col]]),
                                        format = paste("%Y-%m-%d", time_format))
      }
    } else if (inherits(dt[[time_col]], "Period")) {
      # Handle Period objects (from hms())
      dt$detection_time <- dt[[date_col]] + dt[[time_col]]
    } else {
      stop("Unable to parse time column. Please specify time_format or ensure proper datetime format.")
    }
  } else {
    # Try to parse time_col as datetime directly
    if (is.character(dt[[time_col]])) {
      dt$detection_time <- as.POSIXct(dt[[time_col]])
    } else {
      stop("Unable to parse time column. Please provide date_col or ensure time_col is in datetime format.")
    }
  }

  #--------------------------------------------------------------------------- #
  # Create detection intervals and remove duplicates
  dt$detection_interval <- lubridate::floor_date(dt$detection_time, unit = interval_unit)

  # Group columns for deduplication and aggregation
  group_cols <- c(site_col, species_col)
  dedup_cols <- c(group_cols, "detection_interval")

  # Remove duplicates within each interval
  dt_unique <- unique(dt, by = dedup_cols)

  # Calculate VAR
  if (!is.null(recording_length_col) && recording_length_col %in% names(dt)) {
    # Use provided recording days
    var_result <- dt_unique[, .(detections = .N), by = group_cols][
      dt[, .(days_recorded = data.table::first(get(recording_length_col))),
         by = group_cols],
      on = group_cols][
        , var := detections / days_recorded]
  } else {
    # Calculate based on number of unique days
    var_result <- dt_unique[, .(
      detections = .N,
      days_recorded = data.table::uniqueN(as.Date(detection_time))
    ), by = group_cols][
      , var := detections / days_recorded]
  }

  # Convert to dataframe and order by species
  var_result <- as.data.frame(var_result)
  var_result <- var_result[order(var_result[[species_col]]), ]
  rownames(var_result) <- NULL

  return(var_result)
}
