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
#' @param input The input file scheme, i.e. the file scheme you want to change. If auto, renameRes will attempt to detect the scheme.
#' @param output The output file scheme. Any value in BAR, BAR_LT, Audiomoth, SM4
#' @param utc_offset The UTC offset if going from any scheme to BAR_LT
#' @param recordingName The name, or prefix if going from Audiomoth to any other file scheme.
#' @export
#' @examples
#' \dontrun{
#' renameRes(path="path/to/files", output=BAR_LT, utc_offset="0200", recordingName="site5")}
#' @import tools
renameRes <- function(path, copy_path=NULL, recursive=T, input="auto", output, utc_offset=NULL, recordingName=NULL) {

  if(output=="BAR_LT"){
    if(is.null(utc_offset)) {
      stop("utc_offset required to convert to BAR_LT format!")
    }
    if(!is.character(utc_offset)){
      stop(paste0("utc_offset must be a string, please wrap ", utc_offset, " in quotes."))
    }
  }

  # TODO: Add copy files with new name instead of rename!

  files = list.files(path = path, pattern = "\\.wav$|\\.mp3$|\\.flac$", full.names = TRUE,
                     recursive = recursive)

  # End if no files are found and give stop warning
  if (length(files) == 0) {
    stop("No files found in directory")
  }

  patterns <- list(
    "BAR_LT" = list(
      # YYYYMMDDTHHMMSS+{UTC}_recordingName
      regex = "^(\\d{8})T(\\d{6})\\+(\\d{4})_(.*)$",
      components = c("date", "time", "utc_offset", "recordingName")
    ),
    "BAR" = list(
      # YYYYMMDD_HHMMSS_recordingName
      regex = "^(\\d{8})_(\\d{6})_(.*)$",
      components = c("date", "time", "recordingName")
    ),
    "Audiomoth" = list(
      # YYYYMMDD_HHMMSS
      regex = "^(\\d{8})_(\\d{6})$",
      components = c("date", "time")
    ),
    "SM4" = list(
      regex = "^(.*)_(\\d{8})_(\\d{6})$",
      components = c("recordingName", "date", "time")
      # recordingName_YYYYMMDD_HHMMSS
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

  print("Renaming files...")
  extract_components <- function(file, pattern) {
    file = tools::file_path_sans_ext(basename(file))
    matches = regmatches(file, regexec(pattern$regex, file))
    components = setNames(matches[[1]][-1], pattern$components)
    return(components)
  }

  construct_file_name <- function(components, pattern_name, recordingName = recordingName, utc_offset = NULL) {
    if(is.null(recordingName) && "recordingName" %in% names(components)){
      recordingName = components["recordingName"]
    }
    if (pattern_name == "BAR_LT") {
      date_time = paste0(components["date"], "T", components["time"], "+", utc_offset)
      file_name = paste0(date_time, "_", recordingName)
    } else if (pattern_name == "BAR") {
      date_time = paste0(components["date"], "_", components["time"])
      file_name = paste0(date_time, "_", recordingName)
    } else if (pattern_name == "Audiomoth") {
      date_time = paste0(components["date"], "_", components["time"])
      file_name = date_time
    } else if (pattern_name == "SM4") {
      date_time = paste0(components["date"], "_", components["time"])
      file_name = paste0(recordingName, "_", date_time)
    }
    return(file_name)
  }

  current_pattern = patterns[[pattern]]

  file_components = lapply(files, function(file) {
    components = extract_components(file, current_pattern)
  })

  new_file_names = sapply(file_components, function(components) {
    construct_file_name(components, output, recordingName = recordingName,
                        utc_offset = utc_offset)
  })

  new_files = file.path(path,
                          paste0(new_file_names,
                                ".", tools::file_ext(files)))

  if (!is.null(copy_path)) {
    if (!dir.exists(copy_path)) {
      dir.create(copy_path, recursive = TRUE)
    }
    new_files_copy <- file.path(copy_path, new_file_names)
    for (i in seq_along(files)) {
      file.copy(from = files[i], to = new_files_copy[i], overwrite = FALSE)
    }
  } else {
    invisible(file.rename(from = files, to = new_files))
  }

  print("Finished!")
}
