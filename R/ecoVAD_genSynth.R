#' Gets the latest version of BirdNet from github
#'
#' getBN downloads the latest version of BirdNet from github and installs
#' it and the necessary python libraries in the users home directory,
#' unless otherwise specified. Directory ~/BirdNet-Analyzer/BirdNet-Analyzer-main.
#' When specifying a home directory, this same directory must be used as path in
#' analyze().
#'
#' @param audioPath Path to the ecoacoustic training data.
#' @param speechPath Path to the human voices training data
#' @param noisePath Path to the noise training date
#' @param synthPath Path for the synthetic dataset
#' @param
#' @export
#' @examples
#' \dontrun{
#' getBn()
#' getBn(path="C:/projectFolder/")}
#' @importFrom utils download.file unzip
ecoVAD.genSynth <- function(audioPath, speechPath, noisePath, synthPath) {

}
