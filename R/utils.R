#' Get Python Information
#'
#' This function returns information about the version of Python being used by R.
#'
#' @return A list with the values for `py_path`, `py_cmd`, and `venv_activate_cmd`.
get_python_info <- function() {
  os = Sys.info()[["sysname"]]
  py_cmd = ifelse(os == "Windows", "python", c("python3", "python3.7", "python3.8", "python3.9", "python3.10"))
  which_cmd = ifelse(os == "Windows", "where", "which")
  venv_activate_cmd = ifelse(os == "Windows", "Scripts", "bin")

  py_paths = system2(which_cmd, args = c(py_cmd), stdout = TRUE)

  if(!os=="Windows"){
    py_paths = list()
    for(cmd in py_cmd){
      py_paths = append(py_paths, system2(which_cmd, args = c(py_cmd), stdout = TRUE))
    }
  }

  num_paths = length(py_paths)
  for (i in seq_along(py_paths)) {
    path = py_paths[i]
    tryCatch({
      temp_file = tempfile()
      exit_status = system2(path, args = c("--version"), stdout = temp_file, stderr = temp_file)
      if (exit_status != 0 && i == num_paths) {
        stop(paste0("Error: There was a problem checking for Python ", path, ". Exit status: ", exit_status))
      }
      py_version = readLines(temp_file)
      message(paste0(py_version, ' is installed.'))
      found = TRUE
    }, error = function(e) {
      message(e$message)
    })
    if (grepl("([3].[7-9][0-9]?.*|3.10.*)", py_version)) {
      py_path = path
      py_version = py_version
      message("Using Python version: ", py_version)
      return(list(py_path = py_path, py_cmd = py_cmd, venv_activate_cmd = venv_activate_cmd))
    }
  }
  warning("Please install Python 3.7 - 3.10 or ensure that Python is in PATH!")
}
