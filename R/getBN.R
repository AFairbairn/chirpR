#' Gets the latest version of BirdNet from github
#'
#' getBN downloads the latest version of BirdNet from github and installs
#' it and the necessary python libraries in the users home directory,
#' unless otherwise specified. Directory ~/BirdNet-Analyzer/BirdNet-Analyzer-main.
#' When specifying a home directory, this same directory must be used as path in
#' analyze().
#'
#' @param path Path to save BirdNet, defaults to home directory.
#' @param remove Logical, removes the downloaded .zip file, default=True
#' @param ... Other parameters to be passed to download.file()
#' @export
#' @examples
#' \dontrun{
#' getBn()
#' getBn(path="C:/projectFolder/")}
getBN <- function(path, remove = TRUE, ...) {
  # Check path input
  if(missing(path)){
    downloadPath = file.path(path.expand("~"), "BirdNET-Analyzer")
  } else {
    downloadPath = file.path(path, "BirdNET-Analyzer")
  }
  bnPath = file.path(downloadPath, "BirdNet-Analyzer-main")

  # Check for directory, if doesn't exist, create
  if(!dir.exists(downloadPath)){
    cat("Getting BirdNet/n")
    dir.create(downloadPath)
    # Download BirdNet analyzer
    file=file.path(downloadPath, "BirdNET-Analyzer.zip")
    value = download.file("https://github.com/kahst/BirdNET-Analyzer/archive/refs/heads/main.zip",
                          file, ...)
    cat(paste0("Download completed with exit code: ", value))
    unzip(zipfile = file, exdir=downloadPath)
    if(remove){
      unlink(file)
    }
  } else if(dir.exists(bnPath)) {
      cat("BirdNet is already installed. Checking python environment and dependencies.\n")
  } else {
    stop("Oops. Something went wrong! Please check the path and that BirdNet isn't already installed.")
  }

  # Check location of python and python3 executables
  pythonPath = Sys.which("python")
  python3Path = Sys.which("python3")

  # Determine which command to use
  if (nchar(Sys.which("python")) > 0) {
    pythonCmd = "python"
  } else if (nchar(Sys.which("python3")) > 0) {
    pythonCmd = "python3"
  } else {
    stop("Python not found. Please ensure python is installed and added to PATH")
  }

  if (nchar(pythonCmd) > 0) {
    # Get operating system information
    osInfo = Sys.info()
    cat("/n")

    # Check if running on Windows
    if (osInfo["sysname"] == "Windows") {
      # Installing pipenv
      cat("Installing pipenv...\n")
      system("pip install --user pipenv")
      # Create virtual environment using determined command
      cat("Creating virtual environment and installing dependencies...\n")
      oldWd = getwd()
      setwd(bnPath)
      system(paste0("pipenv install librosa numpy tensorflow"))
    } else if (osInfo["sysname"] == "Linux") {
      # Installing pipenv
      system("sudo apt-get install ffmpeg")
      cat("Installing pipenv...\n")
      system("sudo apt install pipenv")
      # Install BirdNet required packages
      cat("Creating virtual environment and installing dependencies...\n")
      system("pipenv install librosa tensorflow")
    }
  } else {
    stop("Python not found. Please install python and ensure it is located in your system path.\n")
  }
  setwd(oldWd)

}
