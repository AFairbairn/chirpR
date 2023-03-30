#' Anonoymizes input data using an ecoVAD model.
#'
#' The anonymisation will by default output .json files for each input file that contains all detections made by the models.
#'
#' \itemize{
#'  \item PATH_INPUT_DATA: # Path to the data to be anonymised
#'  \item PATH_JSON_DETECTIONS: # Path to the result files, namely json files containing the start and end of detections. Note: one json file is created per analysed file. Defaults to working directory/anonymised_data/detections
#'  \item PATH_ANONYMIZED_DATA: # Path to store the anonymised data \(i.e. audiofiles where human speech has clearly been removed\). Defaults to current working directory/anonymised_data.
#'  \item THRESHOLD: # Confidence threshold. Defaults to 0.7
#'  \item FRAME_LENGTH: # A frame must be either 10, 20, or 30 ms in duration. Defaults to 30ms.
#'  \item AGGRESSIVENESS: # aggressiveness mode, which is an integer between 0 and 3. 0 is the least aggressive about filtering out non-speech, 3 is the most aggressive. Defaults to 1.
#'  \item USE_GPU: # Whether ecoVAD and pyannote should be used on GPU. Defaults False.
#'  \item PATH_PARSED_JSON: # Path to where the parsed JSON and .csv file should go.
#'  \item PATH_SAMPLE_DETECTIONS: # Path where the detections should be stored
#'  \item NUMBER_OF_SAMPLES: # Number of samples that should be taken. Defaults to 0.
#'  \item RANDOM_SEED: # Random seed. Defaults to 42.
#' }
#'
#' @param configPath Path to a custom config.yaml file. If not provided, included config file is used and other parameters are required.
#' @param PATH_INPUT_DATA Path to the data to be used for anonymization.
#' @param ECOVAD_WEIGHTS_PATH Path to model weights file. e.g. ecoVAD_ckpt.pt
#' @param ... Other parameters to update in config_data.yaml
#' @export
#' @examples
#' \dontrun{
#' ecoVAD.anonymize()
#' ecoVAD.anonymize(path="C:/projectFolder/confing_inference.yaml")}
ecoVAD.anonymize <- function(configPath, PATH_INPUT_DATA, ECOVAD_WEIGHTS_PATH, ...) {
  if(missing(configPath)){
    if(missing(PATH_INPUT_DATA)){
      stop("Input data path must be provided!")
    }
    if(missing(ECOVAD_WEIGHTS_PATH)){
      stop("Model weights must be provided to anonymize!")
    }
    configPath = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "config_inference.yaml")
    config = yaml::read_yaml(configPath)
    PATH_ANONYMIZED_DATA = file.path(getwd(), "anonymised_data")
    PATH_JSON_DETECTIONS = file.path(PATH_ANONYMIZED_DATA, "detections")
    config$PATH_JSON_DETECTIONS = PATH_JSON_DETECTIONS
    config$PATH_ANONYMIZED_DATA = PATH_ANONYMIZED_DATA
    yaml::write_yaml(config, configPath)
  }
  if(!missing(...)){
    config = yaml::read_yaml(configPath)
    # Get user-provided parameters
    params <- list(...)

    # Update values based on user input
    for (name in names(params)) {
      config[[name]] = params[[name]]
    }
    yaml::write_yaml(config, configPath)
  }
  if (!dir.exists(PATH_ANONYMIZED_DATA)){
    dir.create(PATH_ANONYMIZED_DATA)
  }
  if (!dir.exists(PATH_JSON_DETECTIONS)){
    dir.create(PATH_JSON_DETECTIONS)
  }
  # TODO!
  # Checking for number of samples and creation of folders should nsamples be provided
  # Folder creation, etc.
  # Check python
  python_info = chirpR:::get_python_info()
  package_path = system.file("ecoVAD_chirpR", package = "chirpR")

  message("Anonymizing data...")
  anon_data = file.path(package_path, "anonymise_data.py")
  exit_status = system2(python_info$py_path, args = c(anon_data, "--config", configPath))
  if (exit_status != 0) {
    stop(paste0("Exit status: ", exit_status, " An error occurred while creating the synthetic data or training the model."))
  }
}
