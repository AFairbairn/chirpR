#' Train a custom ecoVAD model
#'
#'
#' Additional parameters to pass to train_model.py must be in the format as in the config file:
#' # Path to store the model weights
#' MODEL_SAVE_PATH: "\{current working directory\}"
#' # Save the checkpoints of early stopping call
#' CKPT_SAVE_PATH: "\{current working directory\}"
#' # Learning rate
#' LR: 0.001
#' # Momentum
#' MOMENTUM: 0.99
#' # Decay
#' DECAY: 0.01
#' # Batch size to use for training
#' BATCH_SIZE: 32
#' # Number of epochs to train the model for
#' NUM_EPOCH: 50
#' # Tensorboard folder
#' TB_PREFIX: "demo_training"
#' # Comment suffix for Tensorboard run
#' TB_COMMENT: "no-comments"
#' # Numbers of workers, best to have num_workers = number of CPUs
#' NUM_WORKERS: 0
#' # Whether to training pipeline should use a GPU
#' USE_GPU: False
#'
#' @param configPath Path to a custom config.yaml file. If not provided, included config file is used.
#' @param TRAIN_VAL_PATH Path to the synthetic training data created with ecoVAD.genSynth(). Only needs to be provided if custom config not used.
#' @param ... Other parameters to update in config_training.yaml
#' @export
#' @examples
#' \dontrun{
#' ecoVAD.train()
#' ecoVAD.train(path="C:/projectFolder/config.yaml")}
#' @import yaml reticulate
ecoVAD.train <- function(configPath, TRAIN_VAL_PATH, ...) {
  if(missing(configPath)){
    if(missing(TRAIN_VAL_PATH)){
      stop("Training value path must be provided")
    }
    configPath = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "config_training.yaml")
    config = yaml::read_yaml(configPath)
    config$TRAIN_VAL_PATH = TRAIN_VAL_PATH
    training_path = file.path(getwd(), "model_weights")
    if (!dir.exists(training_path)){
      dir.create(training_path)
    }
    config$MODEL_SAVE_PATH = file.path(training_path, "ecoVAD_weights.pt")
    config$CKPT_SAVE_PATH = file.path(training_path, "ecoVAD_ckpts.pt")
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
  pyConfig = reticulate::py_config()
    # Check if a virtual environment is active
  venv_path = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "ecoVAD_venv")
  if (pyConfig$virtualenv == venv_path) {
    cat("Virtual environment is active:", config$virtualenv)
  } else {
    cat("Activating ecoVAD virtual environment...")
    use_virtualenv(venv_path)
  }
  # Run train
  make_model = file.path(system.file("ecoVAD_chirpR", package = "chirpR"), "ecovad/train_model.py")
  reticulate::py_run_file(make_model, args = c("--config", configPath))
}
