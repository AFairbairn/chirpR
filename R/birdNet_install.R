#' Gets and installs the latest version of BirdNet from github
#'
#' birdNet.install downloads the latest version of BirdNet from github and installs
#' it and the necessary python libraries using a virtual environment in the package directory,
#' unless otherwise specified. Directory ~/BirdNet-Analyzer/BirdNet-Analyzer-main.
#' When specifying a home directory, this same directory must be used as path in
#' analyze().
#'
#' @param path Path to save BirdNet, defaults to package directory.
#' @param ... Other parameters to be passed to download.file()
#' @export
#' @examples
#' \dontrun{
#' birdNet.install()
#' birdNet.install(path="C:/projectFolder/")}
#' @import utils
birdNet.install <- function(path, ...) {
  # Check path input
  if(missing(path)){
    downloadPath = system.file("birdNet", package = "chirpR")
  } else {
    downloadPath = file.path(path)
  }
  bnPath = file.path(downloadPath, "BirdNet-Analyzer-main")

  # Check python
  python_info = chirpR:::get_python_info()

  # Check for existing BirdNet installation
  if(dir.exists(bnPath)){
    message("BirdNet is already installed. Checking python environment and dependencies.")
  } else {
    message("Getting BirdNet...")
    # Download BirdNet analyzer
    file = file.path(downloadPath, "BirdNET-Analyzer.zip")
    value = download.file("https://github.com/kahst/BirdNET-Analyzer/archive/refs/heads/main.zip",
                          file, ...)
    message(paste0("Download completed with exit code: ", value))
    unzip(zipfile = file, exdir=downloadPath)
    unlink(file)
  }

  # Set the path to the virtual environment
  venv_path = file.path(system.file("birdNet", package = "chirpR"), "birdNet_venv")
  # Load dependencies list
  venv_packagesFile = file.path(system.file("birdNet", package = "chirpR"), "requirements.txt")
  venv_packages = readLines(venv_packagesFile)

  # Check if the virtual environment already exists
   if (dir.exists(venv_path)) {
    message("Checking virtual environment...")
    # Virtual environment exists, check if packages are installed
    pip_path = file.path(venv_path, python_info$venv_activate_cmd, "pip")
    py_venv_path = file.path(venv_path, python_info$venv_activate_cmd, "python.exe")
  } else {
    # Create a new virtual environment
    message("Creating virtual environment...")
    exit_status = system2(python_info$py_path, args = c("-m", "venv", venv_path))
    if (exit_status != 0) {
      stop("An error occurred while creating virtual environment.")
    }
    pip_path <- file.path(venv_path, python_info$venv_activate_cmd, "pip")
  }

  exit_status = system2(pip_path, args = c("install", "-r", venv_packagesFile))
  if (exit_status != 0) {
    stop("An error occurred while creating virtual environment.")
  }
  message("BirdNet install sucessful! BirdNET is ready to use.")

}





