#' Gets and installs the latest version of BirdNet from github
#'
#' birdNet.install downloads the latest version of BirdNet from github and installs
#' it and the necessary python libraries in the users home directory,
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
  os = Sys.info()[["sysname"]]
  py_cmd = ifelse(os == "Windows", "python", "python3")
  which_cmd = ifelse(os == "Windows", "where", "which")
  py_paths = system(paste(which_cmd, py_cmd), intern = TRUE)

  for (path in py_paths) {
    py_version_cmd = paste0(path, " --version")
    tryCatch({
      py_version = system(py_version_cmd, intern = TRUE)
      message(paste0('Python ', py_version, ' is installed.'))
      found <<- TRUE
    }, error = function(e) {
      message(paste0('Warning: There was a problem checking for Python ', version, '.'))
    })
    if (grepl("Python ([3].[7-9][0-9]?.*|3.10.*)", py_version)) {
      py_path = path
      py_version = py_version
      break
    }
  }

  if (length(py_version) == 0) {
    stop("No compatible version of Python found. Please install Python 3.7 to 3.10.")
  }

  message("Using Python version: ", py_version)


  py_cmd <- tolower(py_version)

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
    pip_path <- file.path(venv_path, "Scripts", "pip.exe")
    system2(pip_path, args = c("install", venv_packages))
  } else {
    # Create a new virtual environment
    message("Creating virtual environment...")
    system2(py_path, args = c("-m", "venv", venv_path))
    #system(paste0(py_path, " -m venv ", venv_path))
    pip_path <- file.path(venv_path, "Scripts", "pip.exe")
    system2(pip_path, args = c("install", venv_packages))
    #system2(py_path, args = c("-m", "pip", "install", venv_packages), env = c("VIRTUAL_ENV" = venv_path))
  }

  message("Done! BirdNET is ready to use.")
}





