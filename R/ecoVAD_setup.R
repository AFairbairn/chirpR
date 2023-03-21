#' Creates or sets up a python virtual environment for running ecoVAD functions
#'
#' Creates a python virtual environment in the package directory and installs the
#' required python dependencies for ecoVAD to function.
#'
#' @export
#' @examples
#' \dontrun{
#' ecoVAD.setup()}
ecoVAD.setup <- function() {

  # Check python
  python_info = chirpR:::get_python_info()

  # Set the path to the virtual environment
  venv_path = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "ecoVAD_venv")
  # Load dependencies list
  venv_packagesFile = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "requirements.txt")
  #venv_packages = readLines(venv_packagesFile)

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

  exit_status = system2(pip_path, args = c("install", "-r", venv_packagesFile, "--use-pep517"))
  if (exit_status != 0) {
    stop("An error occurred while creating virtual environment.")
  }

  # Extract the major and minor version numbers from py_version
  version_numbers = strsplit(python_info$py_version, "\\.")[[1]]
  major_version = version_numbers[1]
  minor_version = version_numbers[2]
  # Set the path to the site-packages directory
  site_packages_path = file.path(venv_path, "lib", paste0("python", major_version, ".", minor_version), "site-packages")
  # Set the path to our .pth file
  pth_file_path = file.path(site_packages_path, "my_package.pth")
  # Set the path to our top-level package
  package_path = system.file("ecoVAD_chirpR", package = "chirpR")

  # Write the path to our top-level package to our .pth file
  writeLines(package_path, pth_file_path)

  message("ecoVAD setup sucessful! You may now use ecoVAD functions.")

  if (!.Platform$OS.type == "windows") {
    message("The libsndfile library is required for ecoVAD please ensure it is installed.")
  }
  message("ffmpeg is required for ecoVAD please ensure it is installed.")

}
