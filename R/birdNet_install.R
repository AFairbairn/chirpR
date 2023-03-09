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
#' @importFrom utils download.file unzip
birdNet.install <- function(path, ...) {
  # Check path input
  if(missing(path)){
    downloadPath = system.file("birdNet", package = "chirpR")
  } else {
    downloadPath = file.path(path)
  }
  bnPath = file.path(downloadPath, "BirdNet-Analyzer-main")

  # Check python
  py = reticulate::py_config()

  if(!compareVersion(py$version, "3.8") >=0){
    stop(paste0("BirdNet requires python version 3.8 or greater. You have version ",
                py.version, ". Please upgrade!"))
  }

  # Check for existing BirdNet installation
  if(dir.exists(bnPath)){
    cat("BirdNet is already installed. Checking python environment and dependencies.\n")
  } else {
    cat("Getting BirdNet...\n")
    # Download BirdNet analyzer
    file=file.path(downloadPath, "BirdNET-Analyzer.zip")
    value = download.file("https://github.com/kahst/BirdNET-Analyzer/archive/refs/heads/main.zip",
                          file, ...)
    cat(paste0("Download completed with exit code: ", value, "\n"))
    unzip(zipfile = file, exdir=downloadPath)
    unlink(file)
  }

  # Set the path to the virtual environment
  venv_path = file.path(system.file("birdNet", package = "chirpR"), "birdNet_venv")
  # Load dependencies list
  venv_packagesFile = file.path(system.file("birdNet", package = "chirpR"), "dependencies.txt")
  venv_packages = readLines(venv_packagesFile)

  # Check if the virtual environment already exists
  if (dir.exists(venv_path)) {
    # Virtual environment exists, check if packages are installed
    reticulate::use_virtualenv(virtualenv = venv_path)
    # Check which packages are already installed
    installed <- sapply(venv_packages, reticulate::py_module_available)
    # Remove installed packages from the list
    packages_to_install <- venv_packages[!installed]
    # Install remaining packages
    if (length(packages_to_install) > 0) {
      message("The following Python packages will be installed: ", paste(packages_to_install, collapse = ", "))
      reticulate::py_install(packages_to_install)
    }
  } else {
    # Create a new virtual environment
    cat("Creating virtual environment...\n")
    reticulate::virtualenv_create(envname = venv_path)

    message("The following Python packages will be installed: ", paste(venv_packages, collapse = ", "))
    reticulate::py_install(venv_packages)
  }

}
