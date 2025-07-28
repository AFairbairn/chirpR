#' Create Validation Data from Detection List (Parallel Version)
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
#' @param additional_cols Character vector. Names of additional columns from input data to include in output (default: NULL)
#' @param n_cores Integer. Number of CPU cores to use. If NULL, uses all available cores minus 1 (default: NULL)
#'
#' @return Invisibly returns the path to the created validation CSV file
#'
#' @export
#' @importFrom av av_audio_convert av_media_info
#' @importFrom parallel detectCores makeCluster clusterExport parLapply stopCluster
segment_audio <- function(birdnet_results,
                          output_dir,
                          padding = 2,
                          duration = 3,
                          input_format = "auto",
                          output_format = "auto",
                          file_path_col = "filepath",
                          start_time_col = "start_time",
                          additional_cols = NULL,
                          n_cores = NULL) {

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

  # Validate additional_cols parameter
  if (!is.null(additional_cols) && !is.character(additional_cols)) {
    stop("additional_cols must be a character vector or NULL")
  }

  # Set up parallel processing
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }

  cat("Using", n_cores, "CPU cores for parallel processing\n")

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

  # Validate additional_cols against available columns
  if (!is.null(additional_cols)) {
    missing_additional_cols <- additional_cols[!additional_cols %in% names(df)]
    if (length(missing_additional_cols) > 0) {
      warning("The following additional columns were not found in input data and will be ignored: ",
              paste(missing_additional_cols, collapse = ", "))
      additional_cols <- additional_cols[additional_cols %in% names(df)]
    }
    if (length(additional_cols) > 0) {
      cat("Additional columns to include:", paste(additional_cols, collapse = ", "), "\n")
    }
  }

  # Detect input format if auto
  if (input_format == "auto") {
    # BirdNET table format
    table_cols <- c("filepath", "start_time", "end_time", "common_name", "confidence")
    # Audacity format
    audacity_cols <- c("filepath", "start_time", "end_time", "common_name")
    # Kaleidoscope format - handle both original and R-converted column names
    kaleidoscope_cols_original <- c("INDIR", "FOLDER", "IN FILE", "OFFSET")
    kaleidoscope_cols_r <- c("INDIR", "FOLDER", "IN.FILE", "OFFSET")

    if (all(table_cols %in% names(df))) {
      input_format <- "table"
      cat("Auto-detected input format: table (BirdNET)\n")
    } else if (all(audacity_cols %in% names(df))) {
      input_format <- "audacity"
      cat("Auto-detected input format: audacity\n")
    } else if (all(kaleidoscope_cols_original %in% names(df)) || all(kaleidoscope_cols_r %in% names(df))) {
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
    # Handle both original column names and R-converted names (spaces become dots)
    required_cols_original <- c("INDIR", "FOLDER", "IN FILE", "OFFSET")
    required_cols_r <- c("INDIR", "FOLDER", "IN.FILE", "OFFSET")

    # Check which format we have
    if (all(required_cols_original %in% names(df))) {
      # Original format with spaces
      missing_cols <- required_cols_original[!required_cols_original %in% names(df)]
      in_file_col <- "IN FILE"
      manual_id_col <- "MANUAL ID"
    } else if (all(required_cols_r %in% names(df))) {
      # R-converted format with dots
      missing_cols <- required_cols_r[!required_cols_r %in% names(df)]
      in_file_col <- "IN.FILE"
      manual_id_col <- "MANUAL.ID"
    } else {
      stop("Missing required columns for kaleidoscope format. Need either: ",
           paste(required_cols_original, collapse = ", "), " OR ",
           paste(required_cols_r, collapse = ", "))
    }

    if (length(missing_cols) > 0) {
      stop("Missing required columns for kaleidoscope format: ", paste(missing_cols, collapse = ", "))
    }

    # Construct full path from INDIR/FOLDER/IN FILE (or IN.FILE)
    df$full_path <- file.path(df$INDIR, df$FOLDER, df[[in_file_col]])
    df$detection_start <- df$OFFSET

    # Handle MANUAL ID column (might be MANUAL.ID)
    if (manual_id_col %in% names(df)) {
      df$common_name <- df[[manual_id_col]]
    } else {
      df$common_name <- ""
    }

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

  # Function to extract additional column values
  get_additional_col_values <- function(row_data, cols) {
    if (is.null(cols) || length(cols) == 0) {
      return(list())
    }

    result <- list()
    for (col in cols) {
      if (col %in% names(row_data)) {
        result[[col]] <- row_data[[col]]
      } else {
        result[[col]] <- NA
      }
    }
    return(result)
  }

  # Optimized cut_wav function using av package
  cut_wav <- function(row_data) {
    tryCatch({
      wav_path <- row_data$full_path

      if (!file.exists(wav_path)) {
        return(list(error = paste("WAV file not found:", wav_path)))
      }

      # Calculate start and end times
      start_time <- max(0, row_data$detection_start - padding)
      total_duration <- duration + 2 * padding

      # Create output filename and path
      new_wav_name <- paste0(row_data$unique_id, ".wav")
      new_wav_path <- file.path(wav_output_dir, new_wav_name)

      # Use av package to extract segment directly - much faster!
      av::av_audio_convert(
        audio = wav_path,
        output = new_wav_path,
        start_time = start_time,
        total_time = total_duration,
        verbose = FALSE
      )

      # Get additional column values
      additional_values <- get_additional_col_values(row_data, additional_cols)

      # Return standardized row info with additional columns
      result <- list(
        unique_id = row_data$unique_id,
        original_file = basename(row_data$full_path),
        original_dir = dirname(row_data$full_path),
        new_file_path = new_wav_path,
        new_file_name = new_wav_name,
        original_start_time = row_data$detection_start,
        clip_duration = total_duration,
        padding_used = padding,
        common_name = if("common_name" %in% names(row_data)) row_data$common_name else "Unknown",
        scientific_name = if("scientific_name" %in% names(row_data)) row_data$scientific_name else NA,
        confidence = if("confidence" %in% names(row_data)) row_data$confidence else NA
      )

      # Add additional column values to result
      result <- c(result, additional_values)

      return(result)

    }, error = function(e) {
      return(list(error = paste("Error processing row", row_data$unique_id, ":", e$message)))
    })
  }

  # Process files in parallel
  cat("Processing", nrow(df), "audio files using", n_cores, "cores...\n")

  # Create cluster with proper cleanup
  cl <- NULL
  results <- tryCatch({
    cl <- parallel::makeCluster(n_cores)

    # Export necessary variables and functions to cluster
    parallel::clusterExport(cl, c("cut_wav", "get_additional_col_values", "wav_output_dir",
                                  "padding", "duration", "additional_cols"), envir = environment())

    # Load required packages on each worker
    parallel::clusterEvalQ(cl, {
      library(av)
    })

    # Convert dataframe to list of rows for parallel processing
    df_list <- lapply(seq_len(nrow(df)), function(i) df[i, ])

    # Process in parallel
    start_time <- Sys.time()
    results <- parallel::parLapply(cl, df_list, cut_wav)
    end_time <- Sys.time()

    cat("Parallel processing completed in", round(as.numeric(end_time - start_time), 2), "seconds\n")

    results

  }, finally = {
    # Ensure cluster is always stopped, even if there's an error
    if (!is.null(cl)) {
      try(parallel::stopCluster(cl), silent = TRUE)
      cl <- NULL
    }
    # Force garbage collection to clean up connections
    gc()
  })


  # Filter out errors and extract successful results
  successful_results <- results[!sapply(results, function(x) "error" %in% names(x))]
  errors <- results[sapply(results, function(x) "error" %in% names(x))]

  # Report errors
  if (length(errors) > 0) {
    cat("Errors encountered:\n")
    for (err in errors) {
      cat("  -", err$error, "\n")
    }
  }

  if (length(successful_results) == 0) {
    stop("No audio files were successfully processed")
  }

  # Convert to data frame - handle variable column numbers
  results_df <- do.call(rbind, lapply(successful_results, function(x) {
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
      scientific_name = results_df$scientific_name,
      confidence = results_df$confidence,
      stringsAsFactors = FALSE
    )

    # Add additional columns if they exist
    if (!is.null(additional_cols) && length(additional_cols) > 0) {
      for (col in additional_cols) {
        if (col %in% names(results_df)) {
          out_df[[col]] <- results_df[[col]]
        }
      }
    }

    output_filename <- "validation_table.csv"

  } else if (output_format == "audacity") {
    # Audacity format
    out_df <- data.frame(
      filepath = results_df$new_file_path,
      start_time = 0,
      end_time = results_df$clip_duration,
      common_name = results_df$common_name,
      scientific_name = results_df$scientific_name,
      confidence = results_df$confidence,
      stringsAsFactors = FALSE
    )

    # Add additional columns if they exist
    if (!is.null(additional_cols) && length(additional_cols) > 0) {
      for (col in additional_cols) {
        if (col %in% names(results_df)) {
          out_df[[col]] <- results_df[[col]]
        }
      }
    }

    output_filename <- "validation_audacity.txt"

  } else if (output_format == "kaleidoscope") {
    # Kaleidoscope format
    out_df <- data.frame(
      INDIR = ".",
      FOLDER = "wav_files",
      IN_FILE = results_df$new_file_name,    # Use underscore temporarily
      OFFSET = 0,
      DURATION = results_df$clip_duration,
      MANUAL_ID = "",                        # Use underscore temporarily
      scientific_name = results_df$scientific_name,
      common_name = results_df$common_name,
      confidence = results_df$confidence,
      original_file = results_df$original_file,
      stringsAsFactors = FALSE
    )

    # Explicitly set column names with spaces
    names(out_df)[names(out_df) == "IN_FILE"] <- "IN FILE"
    names(out_df)[names(out_df) == "MANUAL_ID"] <- "MANUAL ID"

    # Add additional columns if they exist
    if (!is.null(additional_cols) && length(additional_cols) > 0) {
      for (col in additional_cols) {
        if (col %in% names(results_df)) {
          out_df[[col]] <- results_df[[col]]
        }
      }
    }

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
      scientific_name = results_df$scientific_name,
      confidence = results_df$confidence,
      stringsAsFactors = FALSE
    )

    # Add additional columns if they exist
    if (!is.null(additional_cols) && length(additional_cols) > 0) {
      for (col in additional_cols) {
        if (col %in% names(results_df)) {
          out_df[[col]] <- results_df[[col]]
        }
      }
    }

    output_filename <- "validation_data.csv"
  }

  # Write output file
  out_csv_path <- file.path(output_dir, output_filename)

  if (output_format == "audacity") {
    # Audacity format is tab-separated without headers - only core columns for compatibility
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

  if (!is.null(additional_cols) && length(additional_cols) > 0) {
    cat("Additional columns included:", paste(additional_cols, collapse = ", "), "\n")
  }

  return(invisible(out_csv_path))
}
