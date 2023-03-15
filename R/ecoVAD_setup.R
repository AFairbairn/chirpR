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
    pip_path <- file.path(venv_path, "Scripts", "pip.exe")
    py_venv_path <- file.path(venv_path, "Scripts", "python.exe")
  } else {
    # Create a new virtual environment
    message("Creating virtual environment...")
    system2(python_info$py_path, args = c("-m", "venv", venv_path))
    pip_path <- file.path(venv_path, python_info$venv_activate_cmd, "pip.exe")
  }
  tryCatch({
    system2(pip_path, args = c("install", "-r", venv_packagesFile, "--use-pep517"))
  }, error = function(e) {
    stop(paste0("An error occurred: ", e))
  })
  message("ecoVAD setup sucessful! You may now use ecoVAD functions.")
}
