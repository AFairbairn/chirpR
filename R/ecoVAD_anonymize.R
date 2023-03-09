#' Generates synthetic data for training and testing an ecoVAD model
#'
#' Additional parameters to pass to train_model.py must be in the format as in the config file:
#' AUDIO_PATH: "./assets/demo_data/training_model/soundscape_data/" ** Only accepts .wav and .mp3!
#' # Path to the directory containing human speech files
#' SPEECH_DIR: "./assets/demo_data/training_model/human_voices/"
#' # Path to the directory containing noise (e.g. environmental noises, animal vocalization)
#' NOISE_DIR: "./assets/demo_data/training_model/natural_sounds/"
#' # Length of the output audio file (in ms)
#' LENGTH_SEGMENTS: 3
#' # Probability of including human speech on a given segment
#' PROBA_SPEECH: 0.5
#' # When speech is added, what is the probability of adding noise to the segment
#' PROBA_NOISE_WHEN_SPEECH: 0.5
#' # When speech is added, what is the probability of adding noise to the segment
#' PROBA_NOISE_WHEN_NO_SPEECH: 0.9
#' # Path to the folder storing the segments
#' AUDIO_OUT_DIR: "./assets/demo_data/training_model/synthetic_dataset"
#' # Whether the dataset should include background noises
#' INCLUDE_NOISES: True
#' # Whether the dataset should include the soundscape
#' INCLUDE_SOUNDSCAPE: True
#'
#' @param configPath Path to a custom config.yaml file. If not provided, included config file is used.
#' @param PATH_INPUT_DATA Path to output synthetic training data. Defaults to current working directory/symthetic_dataset if not provided and no custom config file is used.
#' @param ... Other parameters to update in config_data.yaml
#' @export
#' @examples
#' \dontrun{
#' ecoVAD.genSynth()
#' ecoVAD.genSynth(path="C:/projectFolder/config.yaml")}
#' @importFrom utils download.file unzip
ecoVAD.anonymize <- function(configPath, PATH_INPUT_DATA, ...) {
  if(missing(configPath)){
    if(missing(PATH_INPUT_DATA)){
      stop("Input data path must be provided")
    }
    configPath = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "config_inference.yaml")
    config = yaml::read_yaml(configPath)
    outPath = file.path(getwd(), "anonymised_data")
    config$PATH_JSON_DETECTIONS = outPath
    config$PATH_ANONYMIZED_DATA = outPath
    if (!dir.exists(outPath)){
      dir.create(outPath)
    }
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

  # Get Python configuration
  pyConfig = py_config()
  # Check if a virtual environment is active
  venv_path = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "ecoVAD_venv")
  if (pyConfig$virtualenv == venv_path) {
    cat("Virtual environment is active:", config$virtualenv)
  } else {
    cat("Activating ecoVAD virtual environment...")
    use_virtualenv(venv_path)
  }
  # Run make data
  make_data = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "ecovad/make_data.py")
  py_run_file(make_data, args = c("--config", configPath))
}
