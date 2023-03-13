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

  message("Using Python version: ", py_version)

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
    system2(py_venv_path, args = c("-m", "pip", "install", "--upgrade", "pip"))
    system2(pip_path, args = c("install", "-r", venv_packagesFile, "--use-pep517"))
  } else {
    # Create a new virtual environment
    message("Creating virtual environment...")
    system2(py_path, args = c("-m", "venv", venv_path))
    pip_path <- file.path(venv_path, "Scripts", "pip.exe")
    system2(pip_path, args = c("install", "-r", venv_packagesFile))
  }

  message("ecoVAD setup sucessful! You may now use ecoVAD functions.")

}
