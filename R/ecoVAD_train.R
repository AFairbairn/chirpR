#' Train a custom ecoVAD model
#'
#' Create synthetic data nd train an ecoVAD model. If custom config file is provided,
#' Additional parameters to pass to train_model.py must be in the format as in the config file:
#' \itemize{
#'  \item AUDIO_PATH: "./path/to/soundscape_data/" Path to the audio files to be processed (e.g. the ones that come from the field experiment)
#'  \item SPEECH_DIR: "./path/to/human_voices/" Path to the directory containing human speech files
#'  \item NOISE_DIR: "./path/to/natural_sounds/" Path to the directory containing noise (e.g. environmental noises, animal vocalization)
#'  \item LENGTH_SEGMENTS: 3 Length of the output audio file (in ms)
#'  \item PROBA_SPEECH: 0.5 Probability of including human speech on a given segment
#'  \item PROBA_NOISE_WHEN_SPEECH: 0.5 When speech is added, what is the probability of adding noise to the segment
#'  \item PROBA_NOISE_WHEN_NO_SPEECH: 0.9 When speech is added, what is the probability of adding noise to the segment
#'  \item AUDIO_OUT_DIR: "\{current working directory/synthetic_data\}" Path to the folder storing the segments
#'  \item INCLUDE_NOISES: True Whether the dataset should include background noises
#'  \item INCLUDE_SOUNDSCAPE: True Whether the dataset should include the soundscape
#'  \item TRAIN_VAL_PATH: "\{AUDIO_OUT_DIR\}" Path to the dataset containing the training and validation data
#'  \item MODEL_SAVE_PATH: "\{current working directory\}" Path to store the model weights
#'  \item CKPT_SAVE_PATH: "\{current working directory\}" Save the checkpoints of early stopping call
#'  \item LR: 0.001 Learning rate
#'  \item MOMENTUM: 0.99 Momentum
#'  \item DECAY: 0.01 Decay
#'  \item BATCH_SIZE: 32 Batch size to use for training
#'  \item NUM_EPOCH: 50 Number of epochs to train the model for
#'  \item TB_PREFIX: "demo_training" Tensorboard folder
#'  \item TB_COMMENT: "no-comments" Comment suffix for Tensorboard run
#'  \item NUM_WORKERS: 0 Numbers of workers, best to have num_workers = number of CPUs
#'  \item USE_GPU: False Whether to training pipeline should use a GPU
#'}
#' @param configPath Path to a custom config.yaml file. If provided, all other ecoVAD parameters are ignored and it is assumed that the folders exist! If not provided, included config file is used.
#' @param AUDIO_PATH Path to directory with the audio files. Ignored if trainOnly.
#' @param SPEECH_DIR Path to directory with the speech files. Ignored if trainOnly.
#' @param NOISE_DIR Path to directory with the noise files. Ignored if trainOnly.
#' @param AUDIO_OUT_DIR Path to save the synthetic training data. Only needs to be provided if custom config not used.
#' @param ... Other parameters to update in config_training.yaml
#' @param train Logical, to just create synthetic data set to false. Defaults to true.
#' @param trainOnly Logical, use if you have an existing synthetic dataset that you just want to train. AUDIO_OUT_DIR is ignored.
#' @export
#' @examples
#' \dontrun{
#' ecoVAD.train()
#' ecoVAD.train(path="C:/projectFolder/config.yaml")}
#' @import yaml
ecoVAD.train <- function(configPath, AUDIO_PATH, SPEECH_DIR, NOISE_DIR, AUDIO_OUT_DIR, ..., train=TRUE, trainOnly=FALSE) {
  if(missing(configPath)){
    configPath = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "config_training.yaml")
    if(!trainOnly){
      if(missing(AUDIO_PATH)){
        stop("AUDIO_PATH required!")
      }
      if(missing(SPEECH_DIR)){
        stop("SPEECH_DIR required!")
      }
      if(missing(NOISE_DIR)){
        stop("NOISE_DIR required!")
      }
      if(missing(AUDIO_OUT_DIR)){
        AUDIO_OUT_DIR = "./synthetic_data"
      }
    }
    config = yaml::read_yaml(configPath)
    config$AUDIO_PATH = AUDIO_PATH
    config$SPEECH_DIR = SPEECH_DIR
    config$NOISE_DIR = NOISE_DIR
    if(!dir.exists(AUDIO_OUT_DIR)){
      dir.create(AUDIO_OUT_DIR)
    }
    config$AUDIO_OUT_DIR = AUDIO_OUT_DIR
    config$TRAIN_VAL_PATH = AUDIO_OUT_DIR

    if(!missing(...)){
      # Get user-provided parameters
      params = list(...)
      # Update values based on user input
      for (name in names(params)) {
        config[[name]] = params[[name]]
      }

    }
    yaml::write_yaml(config, configPath)

    # Create output file pats if they don't exist
    MODEL_SAVE_PATH = dirname(config$MODEL_SAVE_PATH)
    CKPT_SAVE_PATH = dirname(config$CKPT_SAVE_PATH)
    if(!dir.exists(MODEL_SAVE_PATH)){
      dir.create(MODEL_SAVE_PATH)
    }
    if(!dir.exists(CKPT_SAVE_PATH)){
      dir.create(CKPT_SAVE_PATH)
    }
  }
  package_path = system.file("ecoVAD_chirpR", package = "chirpR")
  # Check if a virtual environment is active
  venv_path = file.path(package_path, "ecoVAD_venv")
  if(!file.exists(file.path(venv_path, "pyvenv.cfg"))){
    stop("Virtual environment not found. Please check installation or use ecoVAD.setup()")
  }

  if (.Platform$OS.type == "windows") {
    # Windows
    py_path = file.path(venv_path, "Scripts", "python")
  } else {
    # macOS/Linux
    py_path = file.path(venv_path, "bin", "python")
  }

  if(trainOnly){
    message("Training ecoVAD model...")
    make_model = file.path(package_path, "VAD_algorithms", "ecovad", "train_model.py")
    exit_status = system2(py_path, args = c("-m", make_model, "--config", configPath))
  } else if(!train){
    # Run make data
    message("Creating synthetic data...")
    make_data = file.path(package_path, "VAD_algorithms", "ecovad", "make_data.py")
    exit_status = system2(py_path, args = c("-m", make_data, "--config", configPath))
  } else {
    # Run train
    message("Creating synthetic data and training model...")
    make_model = file.path(package_path, "train_ecovad.py")
    exit_status = system2(py_path, args = c("-m", make_model, "--config", configPath))
  }
  if (exit_status != 0) {
    stop(paste0("Exit status: ", exit_status, " An error occurred while creating the synthetic data or training the model."))
  }

}
