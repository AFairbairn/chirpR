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
ecoVAD.train <- function(synthPath) {
  if(missing(synthPath)){
    synthPath = file.path(getwd(), "synthetic_dataset")
  }
  if(!dir.exists(synthPath)){
    dir.create(synthPath)
  }

  ckpt_save_path = "../assets/model_weights/ecoVAD_ckpts_demo.pt"
  model_save_path = "../assets/model_weights/ecoVAD_weights_demo.pt"
}
