#' Calculate vocal activity rates (VAR) from bird detection data
#'
#' This function calculates vocal activity rates using different methods and time intervals.
#' VAR represents the rate of vocal detections per unit time, calculated per species.
#' If site information is provided, results can be returned both by species overall
#' and by species per site.
#'
#' @param df A dataframe containing bird detection data.
#' @param method Character string specifying VAR calculation method:
#'   \itemize{
#'     \item "interval_deduplication" (default): Removes duplicate detections within each time interval,
#'           then calculates detections per recording period
#'     \item "total_detections": Uses all detections divided by total recording time
#'     \item "detections_per_day": Calculates average detections per day
#'   }
#' @param interval_unit Character string for time interval grouping. Options: "minute", "15 minutes",
#'   "hour", "day". Default is "minute".
#' @param time_col Character string specifying the column name containing detection time.
#'   This should be the actual time when the detection occurred (not start time + offset).
#' @param date_col Character string specifying the date column name (if separate from time_col).
#' @param species_col Character string specifying the species column name. Default is "scientific_name".
#' @param site_col Character string specifying the site column name (optional).
#'   If provided, results will include both overall and by-site summaries.
#' @param recording_days_col Character string specifying column with total recording days (optional).
#' @param time_format Character string specifying time format if time_col needs parsing.
#'   Common formats: "%H:%M:%S", "%H%M%S", "%Y-%m-%d %H:%M:%S".
#' @param return_by_site Logical. If TRUE and site_col is provided, return results by site
#'   in addition to overall species results. Default is FALSE.
#' @return If return_by_site is FALSE or site_col is NULL: A dataframe with VAR by species.
#'   If return_by_site is TRUE and site_col is provided: A list with two elements:
#'   \itemize{
#'     \item by_species: VAR summarized across all sites per species
#'     \item by_site: VAR calculated separately for each site-species combination
#'   }
#' @export
#' @importFrom data.table setDT copy
#' @importFrom lubridate ymd hms floor_date
#' @examples
#' \dontrun{
#' # Basic usage - species-level VAR only
#' var_result <- vocal_activity(df, species_col = "scientificName")
#'
#' # With 15-minute intervals (matching your original approach)
#' var_result <- vocal_activity(df,
#'                                       method = "interval_deduplication",
#'                                       interval_unit = "15 minutes",
#'                                       time_col = "detection_time",  # Pre-calculated time
#'                                       species_col = "scientific_name",
#'                                       site_col = "site",
#'                                       recording_days_col = "days_recorded",
#'                                       return_by_site = TRUE)
#'
#' # Access results
#' overall_var <- var_result$by_species
#' site_var <- var_result$by_site
#'
#' # Different data format with combined datetime
#' var_result <- vocal_activity(df,
#'                                       method = "total_detections",
#'                                       time_col = "detection_datetime",
#'                                       species_col = "species")
#' }
vocal_activity <- function(df,
                           method = "interval_deduplication",
                           interval_unit = "minute",
                           time_col = "time",
                           date_col = "date",
                           species_col = "scientific_name",
                           site_col = NULL,
                           recording_days_col = NULL,
                           time_format = NULL,
                           return_by_site = FALSE) {

  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data.frame")
  }

  if (nrow(df) == 0) {
    stop("Input dataframe is empty")
  }

  # Check required columns exist
  required_cols <- c(species_col, time_col)
  if (!is.null(site_col)) required_cols <- c(required_cols, site_col)

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Validate method
  valid_methods <- c("interval_deduplication", "total_detections", "detections_per_day")
  if (!method %in% valid_methods) {
    stop(paste("Invalid method. Choose from:", paste(valid_methods, collapse = ", ")))
  }

  # Validate interval_unit
  valid_intervals <- c("minute", "15 minutes", "hour", "day")
  if (!interval_unit %in% valid_intervals) {
    stop(paste("Invalid interval_unit. Choose from:", paste(valid_intervals, collapse = ", ")))
  }

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

  # Create detection intervals
  dt$detection_interval <- lubridate::floor_date(dt$detection_time, unit = interval_unit)

  # Helper function to calculate VAR for a given grouping
  calculate_var_helper <- function(data, group_cols) {
    if (method == "interval_deduplication") {
      # Remove duplicates within each interval
      dedup_cols <- c(group_cols, "detection_interval")
      data_unique <- unique(data, by = dedup_cols)

      if (!is.null(recording_days_col) && recording_days_col %in% names(data)) {
        # Use provided recording days
        var_result <- data_unique[, .(detections = .N), by = group_cols][
          data[, .(days_recorded = data.table::first(get(recording_days_col))),
               by = group_cols],
          on = group_cols][
            , var := detections / days_recorded]
      } else {
        # Calculate based on number of unique days
        var_result <- data_unique[, .(
          detections = .N,
          days_recorded = data.table::uniqueN(as.Date(detection_time))
        ), by = group_cols][
          , var := detections / days_recorded]
      }

    } else if (method == "total_detections") {
      # Use all detections without deduplication
      if (!is.null(recording_days_col) && recording_days_col %in% names(data)) {
        var_result <- data[, .(detections = .N), by = group_cols][
          data[, .(days_recorded = data.table::first(get(recording_days_col))),
               by = group_cols],
          on = group_cols][
            , var := detections / days_recorded]
      } else {
        var_result <- data[, .(
          detections = .N,
          days_recorded = data.table::uniqueN(as.Date(detection_time))
        ), by = group_cols][
          , var := detections / days_recorded]
      }

    } else if (method == "detections_per_day") {
      # Calculate average detections per day
      var_result <- data[, .(
        daily_detections = .N
      ), by = c(group_cols, "date" = as.Date(detection_time))][
        , .(var = mean(daily_detections)), by = group_cols]
    }

    return(var_result)
  }

  # Calculate VAR by species (overall)
  var_by_species <- calculate_var_helper(dt, species_col)

  # Calculate VAR by site if requested
  results <- list()

  if (!is.null(site_col) && return_by_site) {
    var_by_site <- calculate_var_helper(dt, c(site_col, species_col))

    # Convert to data.frames and clean up
    var_by_species <- as.data.frame(var_by_species)
    var_by_site <- as.data.frame(var_by_site)

    # Round VAR to reasonable precision
    var_by_species$var <- round(var_by_species$var, 4)
    var_by_site$var <- round(var_by_site$var, 4)

    # Sort results
    var_by_species <- var_by_species[order(var_by_species[[species_col]]), ]
    var_by_site <- var_by_site[order(var_by_site[[site_col]], var_by_site[[species_col]]), ]

    rownames(var_by_species) <- NULL
    rownames(var_by_site) <- NULL

    results$by_species <- var_by_species
    results$by_site <- var_by_site

    return(results)

  } else {
    # Return only species-level results
    var_by_species <- as.data.frame(var_by_species)
    var_by_species$var <- round(var_by_species$var, 4)
    var_by_species <- var_by_species[order(var_by_species[[species_col]]), ]
    rownames(var_by_species) <- NULL

    return(var_by_species)
  }
}
