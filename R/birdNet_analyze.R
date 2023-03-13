#' Calls the BirdNet analayze function and returns the results as a dataframe
#'
#' BirdNet.analyze is a wrapper for the BirdNet python script analyze.py. It
#' all the same parameters. See below:
#' \itemize{
#' \item i, Path to input file or folder. If this is a file, o needs to be a file too.
#' \item o, Path to output file or folder. If this is a file, i needs to be a file too.
#' \item lat, Recording location latitude. Set -1 to ignore.
#' \item lon, Recording location longitude. Set -1 to ignore.
#' \item week, Week of the year when the recording was made. Values in \[1, 48\] (4 weeks per month). Set -1 for year-round species list.
#' \item slist, Path to species list file or folder. If folder is provided, species list needs to be named "species_list.txt". If lat and lon are provided, this list will be ignored.
#' \item sensitivity, Detection sensitivity; Higher values result in higher sensitivity. Values in \[0.5, 1.5\]. Defaults to 1.0.
#' \item min_conf, Minimum confidence threshold. Values in \[0.01, 0.99\]. Defaults to 0.1.
#' \item overlap, Overlap of prediction segments. Values in \[0.0, 2.9\]. Defaults to 0.0.
#' \item rtype, Specifies output format. Values in \['table', 'audacity', 'r', 'csv'\]. Defaults to 'table' (Raven selection table).
#' \item threads, Number of CPU threads.
#' \item batchsize, Number of samples to process at the same time. Defaults to 1.
#' \item locale, Locale for translated species common names. Values in \['af', 'de', 'it', ...\] Defaults to 'en'.
#' \item sf_thresh, Minimum species occurrence frequency threshold for location filter. Values in \[0.01, 0.99\]. Defaults to 0.03.
#'}
#' For more details see BirdNet-Analyzer on GitHub: https://github.com/kahst/BirdNET-Analyzer
#'
#' If BirdNet was not installed using BirdNet.install() you must set path.
#'
#' @param i Path to input .wav files for BirdNet analysis.
#' @param o Output file location
#' @param rtype The output file type. Defaults to r
#' @param ... Other parameters to be passed to analyze.py
#' @param result Logical, determines if you want to import the result as a dataframe defaults True
#' @param path If specifying the location of BirdNet. If installed with this package, leave blank.
#' @return Nothing or a CSV file with the species detection results
#' @export
#' @examples
#' \dontrun{
#' # Run without saving the result and default output
#' birdNet.analyze(i="D:/acoustic recordings", result=F)
#'
#' # Run with default /bnResults output location
#' results <- birdNet.analyze(i="D:/acoustic recordings")
#'
#' # Custom output location
#' results <- birdNet.analyze(i="D:/acoustic recordings", o="D:/results")}
birdNet.analyze <- function(i, o, rtype="r", ..., result=TRUE, path){
  # Stop message for missing i
  if(missing(i)){
    stop("The argument i is missing. You must set an input directory!")
  }

  # Create output directory if none provided
  if(missing(o)){
    o = "./bnResults"
    if(!dir.exists(o)){
      dir.create(o)
    }
  }

  # Use default BirdNet path if none provided
  if(missing(path)) {
    path = file.path(system.file("birdNet", package = "chirpR"), "BirdNet-Analyzer-main")
  }

  # Check for proper BirdNet installation
  if(!file.exists(file.path(path, "analyze.py"))){
    stop("BirdNet not found. Please check installation or use birdNet.insall()")
  }
  venv_path = file.path(system.file("birdNet", package = "chirpR"), "birdNet_venv")
  if(!file.exists(file.path(venv_path, "pyvenv.cfg"))){
    stop("Virtual environment not found. Please check installation or use birdNet.insall()")
  }

  # Check if rtype is valid
  if(!rtype %in% c("table", "audacity", "r", "csv")) {
    stop("Invalid output type. Must be table, audacity, r or csv. Default is r.")
  }

  # Check that input directory contains .wav files
  if (!any(grepl("\\.(wav|flac|mp3|ogg|m4a)$", list.files(i, recursive=T)))) {
    stop("No audio files found in the input directory.")
  }

  # Prepare list of args to pass to BirdNet
  args = c(list(i=i, o=o, rtype=rtype), list(...))
  argsStr = paste0("--", names(args), " ",
                    ifelse(names(args) %in% c("i", "o"),
                           paste0("\"", unlist(args), "\""),
                           unlist(args)), collapse = " ")


  birdNet = file.path(path, "analyze.py")

  if (.Platform$OS.type == "windows") {
    # Windows
    py_path = file.path(venv_path, "Scripts", "python")
  } else {
    # macOS/Linux
    py_path = file.path(venv_path, "bin", "python")
  }
  system2(py_path, c(birdNet, argsStr))

  if(result) {
    return(combRes(o, rtype, recursive = T))
  }

}
