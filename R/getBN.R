#' gets latest version of BirdNet from github
#'
#' getBN downloads the latest version of BirdNet from github and installs
#' it in the users home directory, unless otherwise specified.
#'
#' @param path Path to save BirdNet, defaults to home directory.
#' @param remove Logical, removes or keeps .zip file, default=True
#' @param ... Other parameters to be passed to download.file()
#' @export
#'
getBN <- function(path=path.expand("~"), remove=TRUE) {
  path=file.path(path, "BirdNET-Analyzer")
  # Check for directory, if doesn't exist, create
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    if(dir.exists(file.path(path, "BirdNet-Analyzer-main"))) {
      stop("BirdNet is already installed")
    }
  }
  # Download BirdNet analyzer
  file=file.path(path, "BirdNET-Analyzer.zip")
  value = download.file("https://github.com/kahst/BirdNET-Analyzer/archive/refs/heads/main.zip",
                file)
  cat(paste0("Download completed with exit code: ", value))
  unzip(zipfile = file, exdir=path)
  if(remove){
    unlink(file)
  }

  # Check location of python and python3 executables
  python_path = Sys.which("python")
  python3_path = Sys.which("python3")

  # Determine which command to use
  if (nchar(python3_path) > 0) {
    python_cmd = "python3"
  } else if (nchar(python_path) > 0) {
    python_cmd = "python"
  } else {
    stop("Python not found")
  }

  # Create virtual environment using determined command
  system(paste0(python_cmd, " -m venv rnet"))

  # Get operating system information
  os_info = Sys.info()

  # Set path to virtual environment
  venv_path = paste0(path, "/rnet")

  # Check if running on Windows
  if (os_info["sysname"] == "Windows") {
    # Activate virtual environment (Windows)
    system(paste0(venv_path, "\\Scripts\\activate"))
    # Install BirdNet required packages
    system("pip install librosa numpy==1.20 tensorflow")

  } else {
    # Activate virtual environment (non-Windows)
    system(paste0("source ", venv_path, "/bin/activate"))
    # Install BirdNet required packages
    system("pip3 install librosa tensorflow")
    system("apt-get install ffmpeg") # This probably won't work and I will have
  }



}
