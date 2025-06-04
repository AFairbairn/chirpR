#' Create Validation Data from Detection List
#'
#' This function processes detection data in various formats and creates audio clips
#' for validation purposes. It supports multiple input formats and output formats
#' for compatibility with different software packages including BirdNET-Analyzer formats.
#'
#' @param birdnet_results Character path to CSV file OR data.frame. Detection data to be processed.
#' @param output_dir Character. Output directory for WAV files and new CSV
#' @param padding Numeric. Padding (seconds) to add to either side of the cut (default: 2)
#' @param duration Numeric. Duration of each clip in seconds (default: 3)
#' @param input_format Character. Format of input data. Options: "table", "kaleidoscope", "csv", "auto" (default: "auto")
#' @param output_format Character. Format of output CSV. Options: "table", "kaleidoscope", "csv", "auto" (default: "auto")
#' @param file_path_col Character. Column name for file paths when input_format = "csv" (default: "filepath")
#' @param start_time_col Character. Column name for start times when input_format = "csv" (default: "start_time")
#'
#' @return Invisibly returns the path to the created validation CSV file
#'
#' @details
#' **Input Formats (BirdNET-Analyzer compatible):**
#' - "table": BirdNET table format with columns: filepath, start_time, end_time, common_name, confidence
#' - "audacity": Audacity labels format with columns: filepath, start_time, end_time, common_name
#' - "kaleidoscope": Kaleidoscope format with columns: INDIR, FOLDER, IN FILE, OFFSET, DURATION, MANUAL ID
#' - "csv": Generic CSV format with customizable column names
#' - "auto": Automatically detects format based on available columns
#'
#' **Output Formats:**
#' - "table": BirdNET table format
#' - "audacity": Audacity labels format
#' - "kaleidoscope": Kaleidoscope format
#' - "csv": Generic CSV format
#'
#' @examples
#' \dontrun{
#' # BirdNET table format input and output
#' segment_audio(
#'   birdnet_results = "birdnet_detections.csv",
#'   output_dir = "validation_output",
#'   input_format = "table",
#'   output_format = "table"
#' )
#'
#' # Kaleidoscope format input, Audacity output
#' segment_audio(
#'   birdnet_results = "kaleidoscope_detections.csv",
#'   output_dir = "validation_output",
#'   input_format = "kaleidoscope",
#'   output_format = "audacity"
#' )
#'
#' # Auto-detect input format with dataframe input
#' segment_audio(
#'   birdnet_results = my_detections_df,
#'   output_dir = "validation_output"
#' )
#' }
#'
#' @export
#' @importFrom tuneR readWave Wave writeWave
segment_audio <- function(birdnet_results,
                          output_dir,
                          padding = 2,
                          duration = 3,
                          input_format = "auto",
                          output_format = "auto",
                          file_path_col = "filepath",
                          start_time_col = "start_time") {

  if (length(missing_packages) > 0) {
    stop(paste("Required packages not available:", paste(missing_packages, collapse = ", "),
               "\nPlease install with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))", sep = ""))
  }

  # Validate inputs
  if (!is.numeric(padding) || padding < 0) {
    stop("Padding must be a non-negative number")
  }

  if (!is.numeric(duration) || duration <= 0) {
    stop("Duration must be a positive number")
  }

  valid_input_formats <- c("table", "audacity", "kaleidoscope", "csv", "auto")
  valid_output_formats <- c("table", "audacity", "kaleidoscope", "csv", "auto")

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

  # Read detection list - handle both file path and dataframe input
  cat("Reading detection list...\n")
  if (is.data.frame(birdnet_results)) {
    df <- birdnet_results
    cat("Using provided dataframe\n")
  } else if (is.character(birdnet_results) && length(birdnet_results) == 1) {
    if (!file.exists(birdnet_results)) {
      stop("Detection list file does not exist: ", birdnet_results)
    }
    df <- read.csv(birdnet_results, stringsAsFactors = FALSE)
    cat("Read CSV file:", birdnet_results, "\n")
  } else {
    stop("birdnet_results must be either a file path (character) or a data.frame")
  }

  # Detect input format if auto
  if (input_format == "auto") {
    # BirdNET table format
    table_cols <- c("filepath", "start_time", "end_time", "common_name", "confidence")
    # Audacity format
    audacity_cols <- c("filepath", "start_time", "end_time", "common_name")
    # Kaleidoscope format
    kaleidoscope_cols <- c("INDIR", "FOLDER", "IN FILE", "OFFSET")

    if (all(table_cols %in% names(df))) {
      input_format <- "table"
      cat("Auto-detected input format: table (BirdNET)\n")
    } else if (all(audacity_cols %in% names(df))) {
      input_format <- "audacity"
      cat("Auto-detected input format: audacity\n")
    } else if (all(kaleidoscope_cols %in% names(df))) {
      input_format <- "kaleidoscope"
      cat("Auto-detected input format: kaleidoscope\n")
    } else if (file_path_col %in% names(df) && start_time_col %in% names(df)) {
      input_format <- "csv"
      cat("Auto-detected input format: csv\n")
    } else {
      stop("Could not auto-detect input format. Available columns: ", paste(names(df), collapse = ", "),
           "\nPlease specify input_format manually.")
    }
  }

  # Auto-detect output format if not specified
  if (output_format == "auto") {
    output_format <- input_format
    if (output_format == "csv") output_format <- "table"  # Default csv to table format
    cat("Auto-detected output format:", output_format, "\n")
  }

  # Standardize input data to common format
  if (input_format == "table") {
    # BirdNET table format: filepath, start_time, end_time, common_name, confidence
    required_cols <- c("filepath", "start_time", "end_time", "common_name", "confidence")
    missing_cols <- required_cols[!required_cols %in% names(df)]
    if (length(missing_cols) > 0) {
      stop("Missing required columns for table format: ", paste(missing_cols, collapse = ", "))
    }
    df$full_path <- df$filepath
    df$detection_start <- df$start_time

  } else if (input_format == "audacity") {
    # Audacity format: filepath, start_time, end_time, common_name
    required_cols <- c("filepath", "start_time", "end_time", "common_name")
    missing_cols <- required_cols[!required_cols %in% names(df)]
    if (length(missing_cols) > 0) {
      stop("Missing required columns for audacity format: ", paste(missing_cols, collapse = ", "))
    }
    df$full_path <- df$filepath
    df$detection_start <- df$start_time
    if (!"confidence" %in% names(df)) df$confidence <- NA

  } else if (input_format == "kaleidoscope") {
    # Kaleidoscope format: INDIR, FOLDER, IN FILE, OFFSET, DURATION, MANUAL ID
    required_cols <- c("INDIR", "FOLDER", "IN FILE", "OFFSET")
    missing_cols <- required_cols[!required_cols %in% names(df)]
    if (length(missing_cols) > 0) {
      stop("Missing required columns for kaleidoscope format: ", paste(missing_cols, collapse = ", "))
    }
    # Construct full path from INDIR/FOLDER/IN FILE
    df$full_path <- file.path(df$INDIR, df$FOLDER, df$`IN FILE`)
    df$detection_start <- df$OFFSET
    if (!"MANUAL ID" %in% names(df)) df$`MANUAL ID` <- ""
    df$common_name <- df$`MANUAL ID`
    if (!"confidence" %in% names(df)) df$confidence <- NA

  } else if (input_format == "csv") {
    # Generic CSV format with user-specified columns
    if (!file_path_col %in% names(df)) {
      stop("File path column '", file_path_col, "' not found in data")
    }
    if (!start_time_col %in% names(df)) {
      stop("Start time column '", start_time_col, "' not found in data")
    }
    df$full_path <- df[[file_path_col]]
    df$detection_start <- df[[start_time_col]]
    if (!"common_name" %in% names(df)) df$common_name <- "Unknown"
    if (!"confidence" %in% names(df)) df$confidence <- NA
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

      # Read the WAV file using tuneR
      wav_file <- tuneR::readWave(wav_path)
      sample_rate <- wav_file@samp.rate

      # Calculate start and end times
      start_time <- max(0, row_data$detection_start - padding)
      end_time <- row_data$detection_start + duration + padding

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
        original_start_time = row_data$detection_start,
        clip_duration = duration + 2 * padding,
        padding_used = padding,
        common_name = if("common_name" %in% names(row_data)) row_data$common_name else "Unknown",
        confidence = if("confidence" %in% names(row_data)) row_data$confidence else NA
      )

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

  # Create output format based on BirdNET specifications
  if (output_format == "table") {
    # BirdNET table format
    out_df <- data.frame(
      filepath = results_df$new_file_path,
      start_time = 0,
      end_time = results_df$clip_duration,
      common_name = results_df$common_name,
      confidence = results_df$confidence,
      stringsAsFactors = FALSE
    )
    output_filename <- "validation_table.csv"

  } else if (output_format == "audacity") {
    # Audacity format
    out_df <- data.frame(
      filepath = results_df$new_file_path,
      start_time = 0,
      end_time = results_df$clip_duration,
      common_name = results_df$common_name,
      stringsAsFactors = FALSE
    )
    output_filename <- "validation_audacity.txt"

  } else if (output_format == "kaleidoscope") {
    # Kaleidoscope format
    out_df <- data.frame(
      INDIR = ".",
      FOLDER = "wav_files",
      `IN FILE` = results_df$new_file_name,
      OFFSET = 0,
      DURATION = results_df$clip_duration,
      `MANUAL ID` = results_df$common_name,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    output_filename <- "validation_kaleidoscope.csv"

  } else if (output_format == "csv") {
    # Generic CSV format
    out_df <- data.frame(
      filepath = results_df$new_file_path,
      start_time = 0,
      duration = results_df$clip_duration,
      original_file = results_df$original_file,
      original_start_time = results_df$original_start_time,
      common_name = results_df$common_name,
      confidence = results_df$confidence,
      stringsAsFactors = FALSE
    )
    output_filename <- "validation_data.csv"
  }

  # Write output file
  out_csv_path <- file.path(output_dir, output_filename)

  if (output_format == "audacity") {
    # Audacity format is tab-separated without headers
    write.table(out_df[, c("start_time", "end_time", "common_name")],
                out_csv_path, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  } else {
    # CSV format for others
    write.csv(out_df, out_csv_path, row.names = FALSE)
  }

  cat("Successfully processed", nrow(results_df), "audio files\n")
  cat("Output file written to:", out_csv_path, "\n")
  cat("WAV files written to:", wav_output_dir, "\n")
  cat("Output format:", output_format, "\n")

  return(invisible(out_csv_path))
}
