#' Renames files from different recorders to match a certain standard.
#'
#' Different ARU manufacturers have different standards for file naming
#' This script is renames to the files from different recorders to match
#' the nameing scheme of one of the other manufacturers, or a custom naming scheme
#' set by the user. *Caution* this assumes all the files in the path are of the same
#' naming pattern/scheme. All files will be renamed!
#'
#' @param path The path to the folder containing the acoustic files you want to rename.
#' @param copy_path If present, path will be the input path and the files will be copied to the copy_path location with the new filename.
#' @param recursive Defaults to True
#' @param input The input file scheme, i.e. the file scheme you want to change. If auto, rename_audio_files will attempt to detect the scheme.
#' @param output The output file scheme. Any value in BAR, BAR_LT, Audiomoth, SM4
#' @param utc_offset The UTC offset if going from any scheme to BAR_LT
#' @param recording_name The name, or prefix if going from Audiomoth to any other file scheme.
#' @param copy_only Logical. If TRUE, files will be copied to the same directory with new names instead of renamed. Defaults to FALSE.
#' @export
#' @examples
#' \dontrun{
#' # Rename files in place
#' rename_audio_files(path="path/to/files", output="BAR_LT", utc_offset="0200", recording_name="site5")
#'
#' # Copy files to new directory with new names
#' rename_audio_files(path="path/to/files", copy_path="path/to/new/location", output="BAR_LT", utc_offset="0200", recording_name="site5")
#'
#' # Copy files in same directory with new names (keeps originals)
#' rename_audio_files(path="path/to/files", output="BAR_LT", utc_offset="0200", recording_name="site5", copy_only=TRUE)
#' }
#' @import tools
rename_audio_files <- function(path, copy_path=NULL, recursive=T, input="auto", output, utc_offset=NULL, recording_name=NULL, copy_only=FALSE) {

  if(output=="BAR_LT"){
    if(is.null(utc_offset)) {
      stop("utc_offset required to convert to BAR_LT format!")
    }
    if(!is.character(utc_offset)){
      stop(paste0("utc_offset must be a string, please wrap ", utc_offset, " in quotes."))
    }
  }

  files = list.files(path = path, pattern = "\\.wav$|\\.mp3$|\\.flac$", full.names = TRUE,
                     recursive = recursive)

  # End if no files are found and give stop warning
  if (length(files) == 0) {
    stop("No files found in directory")
  }

  patterns <- list(
    "BAR_LT" = list(
      # YYYYMMDDTHHMMSS+{UTC}_recording_name
      regex = "^(\\d{8})T(\\d{6})\\+(\\d{4})_(.*)$",
      components = c("date", "time", "utc_offset", "recording_name")
    ),
    "BAR" = list(
      # YYYYMMDD_HHMMSS_recording_name
      regex = "^(\\d{8})_(\\d{6})_(.*)$",
      components = c("date", "time", "recording_name")
    ),
    "Audiomoth" = list(
      # YYYYMMDD_HHMMSS
      regex = "^(\\d{8})_(\\d{6})$",
      components = c("date", "time")
    ),
    "SM4" = list(
      regex = "^(.*)_(\\d{8})_(\\d{6})$",
      components = c("recording_name", "date", "time")
      # recording_name_YYYYMMDD_HHMMSS
    )
  )

  if(input == "auto"){
    file = tools::file_path_sans_ext(basename(files[1]))
    matches <- sapply(sapply(patterns, function(x) x$regex), grepl, x = file)
    pattern <- names(which(matches))
    #matched_pattern <- patterns[matched_pattern_name]
    print(paste0("Detected ", pattern, " file naming pattern."))
  } else {
    if (!(input %in% names(patterns))) {
      stop("Invalid input pattern name")
    }
    pattern <- input
  }

  print("Processing files...")
  extract_components <- function(file, pattern) {
    file = tools::file_path_sans_ext(basename(file))
    matches = regmatches(file, regexec(pattern$regex, file))
    components = setNames(matches[[1]][-1], pattern$components)
    return(components)
  }

  construct_file_name <- function(components, pattern_name, recording_name = recording_name, utc_offset = NULL) {
    if(is.null(recording_name) && "recording_name" %in% names(components)){
      recording_name = components["recording_name"]
    }
    if (pattern_name == "BAR_LT") {
      date_time = paste0(components["date"], "T", components["time"], "+", utc_offset)
      file_name = paste0(date_time, "_", recording_name)
    } else if (pattern_name == "BAR") {
      date_time = paste0(components["date"], "_", components["time"])
      file_name = paste0(date_time, "_", recording_name)
    } else if (pattern_name == "Audiomoth") {
      date_time = paste0(components["date"], "_", components["time"])
      file_name = date_time
    } else if (pattern_name == "SM4") {
      date_time = paste0(components["date"], "_", components["time"])
      file_name = paste0(recording_name, "_", date_time)
    }
    return(file_name)
  }

  current_pattern = patterns[[pattern]]

  file_components = lapply(files, function(file) {
    components = extract_components(file, current_pattern)
  })

  new_file_names = sapply(file_components, function(components) {
    construct_file_name(components, output, recording_name = recording_name,
                        utc_offset = utc_offset)
  })

  # Determine destination path and create full file paths with extensions
  if (!is.null(copy_path)) {
    # Copy to specified directory
    if (!dir.exists(copy_path)) {
      dir.create(copy_path, recursive = TRUE)
    }
    new_files <- file.path(copy_path, paste0(new_file_names, ".", tools::file_ext(files)))

    print(paste0("Copying ", length(files), " files to ", copy_path, "..."))
    for (i in seq_along(files)) {
      success <- file.copy(from = files[i], to = new_files[i], overwrite = FALSE)
      if (!success) {
        warning(paste0("Failed to copy file: ", files[i]))
      }
    }

  } else if (copy_only) {
    # Copy to same directory with new names
    new_files <- file.path(path, paste0(new_file_names, ".", tools::file_ext(files)))

    print(paste0("Copying ", length(files), " files in same directory..."))
    for (i in seq_along(files)) {
      if (files[i] != new_files[i]) {  # Only copy if names are different
        success <- file.copy(from = files[i], to = new_files[i], overwrite = FALSE)
        if (!success) {
          warning(paste0("Failed to copy file: ", files[i]))
        }
      }
    }

  } else {
    # Rename files in place
    new_files <- file.path(path, paste0(new_file_names, ".", tools::file_ext(files)))

    print(paste0("Renaming ", length(files), " files..."))
    success <- file.rename(from = files, to = new_files)
    if (!all(success)) {
      warning("Some files failed to rename")
    }
  }

  print("Finished!")
}
