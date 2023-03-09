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
      response <- readline(prompt = "Do you want to install them now? [y/n] ")

      if (tolower(response) == "y") {
        # Install remaining packages
        reticulate::py_install(packages_to_install)
      } else {
        stop("These packages are required to use ecoVAD.")
      }
    }
  } else {
    # Create a new virtual environment
    cat("Creating virtual environment...\n")
    reticulate::virtualenv_create(envname = venv_path)

    message("The following Python packages will be installed: ", paste(venv_packages, collapse = ", "))
    response <- readline(prompt = "Do you want to install them now? [y/n] ")
    if (tolower(response) == "y") {
      # Install packages
      reticulate::py_install(venv_packages)
    } else {
      stop("These packages are required to use ecoVAD.\n")
    }
  }

  cat("ecoVAD setup sucessful! You may now use ecoVAD functions.")

}
