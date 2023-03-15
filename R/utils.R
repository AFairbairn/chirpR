#' Get Python Information
#'
#' This function returns information about the version of Python being used by R.
#'
#' @return A list with the values for `py_path`, `py_cmd`, and `venv_activate_cmd`.
get_python_info <- function() {
  os = Sys.info()[["sysname"]]
  py_cmd = ifelse(os == "Windows", "python", "python3")
  which_cmd = ifelse(os == "Windows", "where", "which")
  venv_activate_cmd = ifelse(os == "Windows", "Scripts", "bin")
  py_paths = system(paste(which_cmd, py_cmd), intern = TRUE)

  for (path in py_paths) {
    py_version_cmd = paste0(path, " --version")
    tryCatch({
      py_version = system(py_version_cmd, intern = TRUE)
      message(paste0(py_version, ' is installed.'))
      found = TRUE
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

  return(list(py_path = py_path, py_cmd = py_cmd, venv_activate_cmd = venv_activate_cmd))
}
