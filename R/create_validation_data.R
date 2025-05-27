#' Create Validation Data from Detection List
#'
#' This function processes detection data in various formats and creates audio clips
#' for validation purposes. It supports multiple input formats and output formats
#' for compatibility with different software packages.
#'
#' @param detection_list Character. Path to the detection list CSV file
#' @param output_dir Character. Output directory for WAV files and new CSV
#' @param padding Numeric. Padding (seconds) to add to either side of the cut (default: 2)
#' @param duration Numeric. Duration of each clip in seconds (default: 3)
#' @param input_format Character. Format of input data, Same as BirdNET output. Options: "table", "audacity", "kaleidoscope", "csv", "auto" (default: "auto")
#' @param output_format Character. Format of output CSV. Same as BirdNET output. You can convert between formats here. Options: "table", "audacity", "kaleidoscope", "csv", "auto" (default: "auto")
#' @param file_path_col Character. Column name for file paths when input_format = "simple" (default: "file_path")
#' @param start_time_col Character. Column name for start times when input_format = "simple" (default: "start_time")
#'
#' @return Invisibly returns the path to the created validation CSV file
#'
#' @details
#' **Input Formats:**
#' - "kaleidoscope": Expects columns INDIR, FOLDER, IN FILE, OFFSET (and optionally site, common_name, confidence, scientific_name)
#' - "simple": Expects a file path column and start time column (customizable names)
#' - "auto": Automatically detects format based on available columns
#'
#' **Output Formats:**
#' - "kaleidoscope": Compatible with Kaleidoscope software format
#' - "audacity": Creates labels file format for Audacity (file_path, start_time, end_time, label)
#' - "raven": Compatible with Raven Pro software (Begin Time, End Time, Begin File, etc.)
#' - "simple": Basic format (file_path, start_time, duration)
#'
#' @examples
#' \dontrun{
#' # Kaleidoscope format input and output
#' create_validation_data(
#'   detection_list = "kaleidoscope_detections.csv",
#'   output_dir = "validation_output"
#' )
#'
#' # Simple format input with custom column names
#' create_validation_data(
#'   detection_list = "simple_detections.csv",
#'   output_dir = "validation_output",
#'   input_format = "simple",
#'   output_format = "audacity",
#'   file_path_col = "audio_file",
#'   start_time_col = "detection_time"
#' )
#'
#' # Auto-detect input, output for Raven Pro
#' create_validation_data(
#'   detection_list = "detections.csv",
#'   output_dir = "raven_output",
#'   output_format = "raven"
#' )
#' }
#'
#' @export
create_validation_data <- function(detection_list,
                                   output_dir,
                                   padding = 2,
                                   duration = 3,
                                   input_format = "auto",
                                   output_format = "kaleidoscope",
                                   file_path_col = "file_path",
                                   start_time_col = "start_time") {

  # Check for required packages
  required_packages <- c("tuneR", "readr", "dplyr")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop(paste("Required packages not available:", paste(missing_packages, collapse = ", "),
               "\nPlease install with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))", sep = ""))
  }

  # Load required libraries
  library(tuneR)
  library(readr)
  library(dplyr)

  # Validate inputs
  if (!file.exists(detection_list)) {
    stop("Detection list file does not exist: ", detection_list)
  }

  if (!is.numeric(padding) || padding < 0) {
    stop("Padding must be a non-negative number")
  }

  if (!is.numeric(duration) || duration <= 0) {
    stop("Duration must be a positive number")
  }

  valid_input_formats <- c("kaleidoscope", "simple", "auto")
  valid_output_formats <- c("kaleidoscope", "audacity", "raven", "simple")

  if (!input_format %in% valid_input_formats) {
    stop("Invalid input_format. Must be one of: ", paste(valid_input_formats, collapse = ", "))
  }

  if (!output_format %in% valid_output_formats) {
    stop("Invalid output_format. Must be one of: ", paste(valid_output_formats, collapse = ", "))
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create wav_files subdirectory
  wav_output_dir <- file.path(output_dir, "wav_files")
  if (!dir.exists(wav_output_dir)) {
    dir.create(wav_output_dir, recursive = TRUE)
  }

  # Read detection list
  cat("Reading detection list...\n")
  df <- readr::read_csv(detection_list, show_col_types = FALSE)

  # Detect input format if auto
  if (input_format == "auto") {
    kaleidoscope_cols <- c("INDIR", "FOLDER", "IN FILE", "OFFSET")
    if (all(kaleidoscope_cols %in% names(df))) {
      input_format <- "kaleidoscope"
      cat("Auto-detected input format: kaleidoscope\n")
    } else if (file_path_col %in% names(df) && start_time_col %in% names(df)) {
      input_format <- "simple"
      cat("Auto-detected input format: simple\n")
    } else {
      stop("Could not auto-detect input format. Please specify input_format manually.\n",
           "For kaleidoscope format, need columns: ", paste(kaleidoscope_cols, collapse = ", "), "\n",
           "For simple format, need columns: ", file_path_col, ", ", start_time_col)
    }
  }

  # Standardize input data to common format
  if (input_format == "kaleidoscope") {
    required_cols <- c("INDIR", "FOLDER", "IN FILE", "OFFSET")
    missing_cols <- required_cols[!required_cols %in% names(df)]
    if (length(missing_cols) > 0) {
      stop("Missing required columns for kaleidoscope format: ", paste(missing_cols, collapse = ", "))
    }

    # Standardize to common format
    df$full_path <- file.path(df$INDIR, df$FOLDER, df$`IN FILE`)
    df$start_time <- df$OFFSET

  } else if (input_format == "simple") {
    if (!file_path_col %in% names(df)) {
      stop("File path column '", file_path_col, "' not found in data")
    }
    if (!start_time_col %in% names(df)) {
      stop("Start time column '", start_time_col, "' not found in data")
    }

    # Standardize to common format
    df$full_path <- df[[file_path_col]]
    df$start_time <- df[[start_time_col]]
  }

  # Add unique_id column
  df$unique_id <- seq_len(nrow(df))

  # Function to cut individual WAV file
  cut_wav <- function(row_data) {
    tryCatch({
      wav_path <- row_data$full_path

      if (!file.exists(wav_path)) {
        warning("WAV file not found: ", wav_path)
        return(NULL)
      }

      # Read the WAV file
      wav_file <- tuneR::readWave(wav_path)
      sample_rate <- wav_file@samp.rate

      # Calculate start and end times
      start_time <- max(0, row_data$start_time - padding)
      end_time <- row_data$start_time + duration + padding

      start_sample <- max(1, round(start_time * sample_rate))
      end_sample <- min(length(wav_file@left), round(end_time * sample_rate))

      # Extract the segment
      if (wav_file@stereo) {
        left_segment <- wav_file@left[start_sample:end_sample]
        right_segment <- wav_file@right[start_sample:end_sample]
        wav_segment <- tuneR::Wave(left = left_segment, right = right_segment,
                                   samp.rate = sample_rate, bit = wav_file@bit)
      } else {
        left_segment <- wav_file@left[start_sample:end_sample]
        wav_segment <- tuneR::Wave(left = left_segment, samp.rate = sample_rate,
                                   bit = wav_file@bit)
      }

      # Create output filename and path
      new_wav_name <- paste0(row_data$unique_id, ".wav")
      new_wav_path <- file.path(wav_output_dir, new_wav_name)

      # Write the new WAV file
      tuneR::writeWave(wav_segment, new_wav_path)

      # Return standardized row info
      result <- list(
        unique_id = row_data$unique_id,
        original_file = basename(row_data$full_path),
        original_dir = dirname(row_data$full_path),
        new_file_path = new_wav_path,
        new_file_name = new_wav_name,
        original_start_time = row_data$start_time,
        clip_duration = duration + 2 * padding,
        padding_used = padding
      )

      # Add optional fields if they exist
      optional_fields <- c("site", "common_name", "confidence", "scientific_name",
                           "species", "label", "score")
      for (field in optional_fields) {
        if (field %in% names(row_data)) {
          result[[field]] <- row_data[[field]]
        }
      }

      return(result)

    }, error = function(e) {
      warning("Error processing row ", row_data$unique_id, ": ", e$message)
      return(NULL)
    })
  }

  # Process each row
  cat("Processing", nrow(df), "audio files...\n")
  processed_rows <- list()

  for (i in seq_len(nrow(df))) {
    if (i %% 10 == 0) {
      cat("Processed", i, "of", nrow(df), "files\n")
    }

    row_result <- cut_wav(df[i, ])
    if (!is.null(row_result)) {
      processed_rows[[length(processed_rows) + 1]] <- row_result
    }
  }

  if (length(processed_rows) == 0) {
    stop("No audio files were successfully processed")
  }

  # Convert to data frame
  results_df <- do.call(rbind, lapply(processed_rows, function(x) {
    data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  }))

  # Create output format
  if (output_format == "kaleidoscope") {
    out_df <- data.frame(
      site = ifelse("site" %in% names(results_df), results_df$site, ""),
      INDIR = ".",
      FOLDER = "wav_files",
      `IN FILE` = results_df$new_file_name,
      OFFSET = 0,
      DURATION = results_df$clip_duration,
      `MANUAL ID` = ifelse("common_name" %in% names(results_df), results_df$common_name,
                           ifelse("species" %in% names(results_df), results_df$species, "")),
      confidence = ifelse("confidence" %in% names(results_df), results_df$confidence,
                          ifelse("score" %in% names(results_df), results_df$score, "")),
      scientific_name = ifelse("scientific_name" %in% names(results_df), results_df$scientific_name, ""),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    output_filename <- "validation_list.csv"

  } else if (output_format == "audacity") {
    out_df <- data.frame(
      file_path = results_df$new_file_path,
      start_time = 0,
      end_time = results_df$clip_duration,
      label = ifelse("common_name" %in% names(results_df), results_df$common_name,
                     ifelse("species" %in% names(results_df), results_df$species,
                            ifelse("label" %in% names(results_df), results_df$label, "detection"))),
      stringsAsFactors = FALSE
    )
    output_filename <- "audacity_labels.txt"

  } else if (output_format == "raven") {
    out_df <- data.frame(
      Selection = seq_len(nrow(results_df)),
      View = "Spectrogram 1",
      Channel = 1,
      `Begin Time (s)` = 0,
      `End Time (s)` = results_df$clip_duration,
      `Low Freq (Hz)` = 0,
      `High Freq (Hz)` = 22050,  # Will be updated based on actual file sample rate
      `Begin File` = results_df$new_file_name,
      `File Offset (s)` = 0,
      Species = ifelse("common_name" %in% names(results_df), results_df$common_name,
                       ifelse("species" %in% names(results_df), results_df$species, "")),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    output_filename <- "raven_selections.txt"

  } else if (output_format == "simple") {
    out_df <- data.frame(
      file_path = results_df$new_file_path,
      start_time = 0,
      duration = results_df$clip_duration,
      original_file = results_df$original_file,
      original_start_time = results_df$original_start_time,
      label = ifelse("common_name" %in% names(results_df), results_df$common_name,
                     ifelse("species" %in% names(results_df), results_df$species,
                            ifelse("label" %in% names(results_df), results_df$label, "detection"))),
      stringsAsFactors = FALSE
    )
    output_filename <- "simple_validation.csv"
  }

  # Write output file
  out_csv_path <- file.path(output_dir, output_filename)

  if (output_format == "audacity") {
    # Audacity labels format is tab-separated without headers
    write.table(out_df[, c("start_time", "end_time", "label")],
                out_csv_path, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  } else if (output_format == "raven") {
    # Raven format is tab-separated
    write.table(out_df, out_csv_path, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  } else {
    # CSV format for others
    readr::write_csv(out_df, out_csv_path)
  }

  cat("Successfully processed", nrow(results_df), "audio files\n")
  cat("Output file written to:", out_csv_path, "\n")
  cat("WAV files written to:", wav_output_dir, "\n")
  cat("Output format:", output_format, "\n")

  return(invisible(out_csv_path))
}
