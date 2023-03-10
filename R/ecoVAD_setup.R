#' Creates or sets up a python virtual environment for running ecoVAD functions
#'
#' Creates a python virtual environment in the package directory and installs the
#' required python dependencies for ecoVAD to function.
#'
#' @export
#' @examples
#' \dontrun{
#' ecoVAD.setup()}
#' @import reticulate
ecoVAD.setup <- function() {
  py = reticulate::py_config()

  if(!compareVersion(py$version, "3.8") >=0){
    stop(paste0("ecoVAD requires python version 3.8 or greater. You have version ",
                   py.version, ". Please upgrade!"))
  }
  # Set the path to the virtual environment
  venv_path = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "ecoVAD_venv")
  # Load dependencies list
  venv_packagesFile = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "dependencies.txt")
  venv_packages = readLines(venv_packagesFile)

  # Check if the virtual environment already exists
  if (dir.exists(venv_path)) {
    tryCatch({
      # Virtual environment exists, check if packages are installed
      reticulate::use_virtualenv(virtualenv = venv_path)
      # Check which packages are already installed
      installed <- sapply(venv_packages, reticulate::py_module_available)
      # Remove installed packages from the list
      packages_to_install <- venv_packages[!installed]
      # Install remaining packages
      if (length(packages_to_install) > 0) {
        # Ask the user for confirmation
        message("The following Python packages will be installed: ", paste(packages_to_install, collapse = ", "))
        reticulate::virtualenv_install(packages_to_install)
      }
    }, error = function(e) {
      # Handle error when activating virtual environment or installing packages
      mesage(paste0("Error: ", e))
      message("An error occurred while activating the virtual environment or installing packages.")
      stop("Please make sure that Python is installed and available in your PATH.")
    })
  } else {
    tryCatch({
      # Create a new virtual environment
      message("Creating virtual environment...\n")
      reticulate::virtualenv_create(envname = venv_path)
      reticulate::use_virtualenv(venv_path)
      message("The following Python packages will be installed: ", paste(venv_packages, collapse = ", "))
      reticulate::virtualenv_install(venv_packages, envname=venv_path)
    }, error = function(e) {
      # Handle error when creating virtual environment or installing packages
      mesage(paste0("Error: ", e))
      message("An error occurred while creating the virtual environment or installing packages.")
      stop("Please make sure that Python is installed and available in your PATH.")
    })
  }

  message("ecoVAD setup sucessful! You may now use ecoVAD functions.")

}
