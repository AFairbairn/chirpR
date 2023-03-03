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
  path=file.path(path.expand("~"), "BirdNET-Analyzer")
  # Check for directory, if doesn't exist, create
  if(!dir.exists(path)){
    dir.create(path)
  } else {
    if(dir.exists(file.path(path, "BirdNet-Analyzer-main"))) {
      stop("BirdNet is already installed")
    }
  }
  file=file.path(path, "BirdNET-Analyzer.zip")
  value <- download.file("https://github.com/kahst/BirdNET-Analyzer/archive/refs/heads/main.zip",
                file)
  cat(paste0("Download completed with exit code: ", value))
  unzip(zipfile = file, exdir=path)
  if(remove){
    unlink(file)
  }
}
