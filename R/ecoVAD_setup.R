#' Gets the latest version of BirdNet from github
#'
#' getBN downloads the latest version of BirdNet from github and installs
#' it and the necessary python libraries in the users home directory,
#' unless otherwise specified. Directory ~/BirdNet-Analyzer/BirdNet-Analyzer-main.
#' When specifying a home directory, this same directory must be used as path in
#' analyze().
#'
#' @param
#' @export
#' @examples
#' \dontrun{
#' getBn()
#' getBn(path="C:/projectFolder/")}
#' @importFrom reticulate
ecoVAD.setup <- function() {
  userPermission <- utils::askYesNo("Install ecoVAD dependencies?")

  if (isTRUE(userPermission)) {
    reticulate
  } else {
    stop("These packages are required. Please install if you wish to use ecoVAD")
  }
}
