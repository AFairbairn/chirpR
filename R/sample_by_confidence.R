
#' Sample Detections Across Confidence Range for Validation
#'
#' This function samples a specified number of detections evenly across confidence
#' ranges for each species and optionally creates validation audio clips. It supports
#' BirdNET-Analyzer compatible formats and uses stratified sampling across confidence bins.
#'
#' @param birdnet_results Character or data.frame. Either a path to a BirdNET detection file or a data frame containing BirdNET detections.
#' @param n_samples Integer. Target number of samples to select per species (default: 100)
#' @param n_bins Integer. Number of confidence bins to create for stratified sampling (default: 10)
#' @param resample Logical. If TRUE, resample from other bins to reach n_samples when bins are uneven. If FALSE, return equal samples per bin (may be fewer than n_samples) (default: TRUE)
#' @param species Character vector. Species name(s) of interest. If NULL, all species in the data will be processed. (default: NULL)
#' @param species_column Character. Column name containing the species names. (default: "scientific_name")
#' @param create_clips Logical. If TRUE, create validation audio clips using create_validation_data (default: FALSE)
#' @param output_dir Character. Output directory for clips (required if create_clips = TRUE)
#' @param input_format Character. Format of input data. Currently only "kaleidoscope" is supported (default: "kaleidoscope")
#' @param output_format Character. Format of output CSV. Currently only "kaleidoscope" is supported (default: "kaleidoscope")
#' @param seed Integer. Random seed for reproducible sampling (default: NULL)
#' @param min_conf Numeric. Minimum confidence threshold to include (default: 0.1)
#' @param max_conf Numeric. Maximum confidence threshold to include (default: 1.0)
#' @param verbose Logical. If TRUE, show progress messages during processing (default: FALSE)
#' @param ... Additional arguments passed to create_validation_data()
#'
#' @return A list containing:
#'   - sampled_data: Data frame of sampled detections from all processed species
#'   - validation_path: Path to validation files (if create_clips = TRUE, NULL otherwise)
#'
#' @details
#' This function performs stratified sampling across confidence score ranges to ensure
#' representation across the full confidence spectrum for each species separately.
#'
#' **Sampling Strategy:**
#' - Confidence scores are divided into n_bins equal-width bins based on the full dataset range
#' - Each species is sampled separately using the same bin structure
#' - If resample = TRUE: Target n_samples per species, resampling from populated bins if needed
#' - If resample = FALSE: Equal samples per bin per species, total may be less than n_samples
#' - All species results are combined into a single output dataset
#'
#' **Multi-Species Processing:**
#' - When species = NULL, all species in the dataset are processed
#' - Each species gets up to n_samples detections
#' - Final dataset contains samples from all species combined
#'
#' **Input Requirements:**
#' - Data must contain a column with 'confidence' in the name (case-insensitive)
#' - Data must contain the specified species_column
#' - Currently only supports Kaleidoscope format
#'
#' @examples
#' \dontrun{
#' # Sample 50 detections per species across confidence range, create clips
#' result <- sample_by_confidence(
#'   birdnet_results = "birdnet_detections.csv",
#'   n_samples = 50,
#'   n_bins = 5,
#'   create_clips = TRUE,
#'   output_dir = "validation_clips"
#' )
#'
#' # Sample specific species without resampling, no clips
#' result <- sample_by_confidence(
#'   birdnet_results = "detections.csv",
#'   species = c("Turdus migratorius", "Corvus brachyrhynchos"),
#'   n_samples = 100,
#'   resample = FALSE,
#'   create_clips = FALSE
#' )
#'
#' }
#'
#' @export
#'
sample_by_confidence <- function(birdnet_results,
                                 n_samples = 100,
                                 n_bins = 10,
                                 resample = TRUE,
                                 species = NULL,
                                 species_column = "scientific_name",
                                 create_clips = FALSE,
                                 output_dir = NULL,
                                 input_format = "kaleidoscope",
                                 output_format = "kaleidoscope",
                                 seed = NULL,
                                 min_conf = 0.1,
                                 max_conf = 1,
                                 verbose = FALSE,
                                 ...) {

  # Helper function to process a single species
  process_species <- function(species, df, n_samples, n_bins, resample) {

    # Check if we have enough data total
    if (n_samples > nrow(df)) {
      if (verbose) {
        warning(paste("Requested", n_samples, "samples but only", nrow(df),
                      "observations available. Returning all available data."))
      }
      return(df)
    }
    samples_per_bin <- floor(n_samples / n_bins)
    strata_values <- df$conf_bin
    unique_strata <- unique(strata_values)
    sampled_indices <- integer(0)

    # First pass: sample from each stratum
    for (stratum in unique_strata) {
      stratum_indices <- which(strata_values == stratum)
      n_to_sample <- min(samples_per_bin, length(stratum_indices))

      if (n_to_sample > 0) {
        if (length(stratum_indices) == 1) {
          sampled_indices <- c(sampled_indices, stratum_indices)
        } else {
          sampled_indices <- c(sampled_indices,
                               sample(stratum_indices, n_to_sample, replace = FALSE))
        }
      }
    }


    # Second pass: redistribute if resample=TRUE
    if (resample && length(sampled_indices) < n_samples) {
      remaining_needed <- n_samples - length(sampled_indices)
      unused_indices <- setdiff(seq_len(nrow(df)), sampled_indices)

      if (length(unused_indices) > 0) {
        unused_strata <- split(unused_indices, strata_values[unused_indices])
        unused_strata <- unused_strata[lengths(unused_strata) > 0]

        if (length(unused_strata) > 0) {
          extra_per_stratum <- ceiling(remaining_needed / length(unused_strata))
          additional_indices <- integer(0)

          for (stratum_unused in unused_strata) {
            n_extra <- min(extra_per_stratum, length(stratum_unused))
            if (n_extra > 0 && length(additional_indices) < remaining_needed) {
              additional_indices <- c(additional_indices,
                                      sample(stratum_unused, n_extra, replace = FALSE))
            }
          }

          # Trim to exact amount needed
          if (length(additional_indices) > remaining_needed) {
            additional_indices <- additional_indices[1:remaining_needed]
          }

          sampled_indices <- c(sampled_indices, additional_indices)
        }
      }
    }

    # Final check and optional warning
    result <- df[sampled_indices, , drop = FALSE]

    if (verbose && nrow(result) < n_samples) {
      warning(paste("Requested", n_samples, "samples but only", nrow(result),
                    "could be obtained for", species, "with current stratification constraints."))
    }

    return(result)
  }

  # -------------------------------------------------------------------------- #
  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Validate inputs
  if (!is.numeric(n_samples) || n_samples <= 0) {
    stop("n_samples must be a positive integer")
  }

  if (!is.numeric(n_bins) || n_bins <= 0) {
    stop("n_bins must be a positive integer")
  }

  if (!is.logical(resample)) {
    stop("resample must be TRUE or FALSE")
  }

  if (!is.logical(create_clips)) {
    stop("create_clips must be TRUE or FALSE")
  }

  if (create_clips && is.null(output_dir)) {
    stop("output_dir must be specified when create_clips = TRUE")
  }

  if (!is.numeric(min_conf) || !is.numeric(max_conf)) {
    stop("min_conf and max_conf must be numeric")
  }

  if (min_conf >= max_conf) {
    stop("min_conf must be less than max_conf")
  }

  # -------------------------------------------------------------------------- #
  # To do: Add handling of other BirdNET output types
  # Read detection list
  if (is.character(birdnet_results)) {
    df <- read.csv(birdnet_results, stringsAsFactors = FALSE)
  } else if (is.data.frame(birdnet_results)) {
    df <- birdnet_results
  } else {
    stop("birdnet_results must be either a file path (character) or a data frame.")
  }

  # Look for any column name that contains 'confidence' (case-insensitive)
  confidence_matches <- grep("confidence", names(df), ignore.case = TRUE, value = TRUE)

  if (length(confidence_matches) == 0) {
    stop("No column containing 'confidence' found (case-insensitive)")
  } else if (length(confidence_matches) > 1) {
    stop(paste("Multiple columns containing 'confidence' found:", paste(confidence_matches, collapse = ", ")))
  } else {
    df$conf_score <- df[[confidence_matches]]
  }

  # Filter by confidence range
  original_count <- nrow(df)
  df <- df[df$conf_score >= min_conf & df$conf_score <= max_conf, ]
  df <- df[!is.na(df$conf_score), ]

  if (nrow(df) == 0) {
    stop("No detections found within confidence range [", min_conf, ", ", max_conf, "]")
  }

  # Check if species column exists
  if (!species_column %in% names(df)) {
    stop("Species column '", species_column, "' not found in data")
  }

  # Get list of available species
  available_species <- unique(df[[species_column]])

  # Handle species filtering/validation
  if (!is.null(species)) {
    # Ensure species is a character vector
    species <- as.character(species)

    # Check for invalid species
    invalid_species <- setdiff(species, available_species)
    if (length(invalid_species) > 0) {
      stop("The following species are not found in the data: ",
           paste(invalid_species, collapse = ", "))
    }

    # Filter to the valid species
    available_species <- species
  }

  min_conf_data <- min(df$conf_score)
  max_conf_data <- max(df$conf_score)

  # Create bin breaks with a small buffer to ensure all values are included
  bin_width <- (max_conf_data - min_conf_data) / n_bins
  bin_breaks <- seq(min_conf_data, max_conf_data + bin_width * 0.0001, length.out = n_bins + 1)

  # Ensure the last break is definitely larger than the max value
  bin_breaks[length(bin_breaks)] <- max(bin_breaks[length(bin_breaks)], max_conf_data + 1e-10)

  # Apply bins to ALL data ONCE and don't recalculate
  df$conf_bin <- cut(df$conf_score, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)

  # Initialize combined results
  all_sampled_rows <- list()

  # Process each species with the SAME bins
  for (current_species in available_species) {

    species_df <- df[df[[species_column]] == current_species, ]

    if (nrow(species_df) == 0) {
      warning("No data for species: ", current_species)
      next
    }

    # Sample this species using the global bins
    species_sampled <- process_species(current_species, species_df, n_samples, n_bins, resample)

    if (!is.null(species_sampled) && nrow(species_sampled) > 0) {
      all_sampled_rows[[current_species]] <- species_sampled
    }
  }

  # Combine all sampled data
  if (length(all_sampled_rows) == 0) {
    stop("No samples were selected from any species. Check your sampling parameters.")
  }

  final_sampled_df <- do.call(rbind, all_sampled_rows)
  rownames(final_sampled_df) <- NULL

  if (!identical(levels(final_sampled_df$conf_bin), levels(df$conf_bin))) {
    warning("Confidence bin levels changed during sampling - this indicates a bug!")
  }

  result = final_sampled_df

  # Create validation clips if requested (for ALL combined results)
  if (create_clips) {
    if (verbose) {
      cat("Creating validation audio clips for all selected samples...\n")
      flush.console()
    }

    # Save combined sampled detections to temporary file
    temp_file <- tempfile(fileext = ".csv")
    utils::write.csv(final_sampled_df, temp_file, row.names = FALSE)

    # Call create_validation_data with ALL the sampled detections
    validation_path <- create_validation_data(
      birdnet_results = temp_file,
      output_dir = output_dir,
      input_format = input_format,
      output_format = output_format,
      ...
    )

    result <- list(
      sampled_data = final_sampled_df,
      validation_path = validation_path
    )

    # Clean up temporary file
    unlink(temp_file)
  }

  return(result)
}
