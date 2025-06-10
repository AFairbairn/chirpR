#' Calculate confidence thresholds
#'
#' This function fits logistic regression models to calculate confidence thresholds
#' for each species validated. It can handle single or multiple species and
#' returns either just thresholds or complete model results with predictions.
#'
#' @param df A data frame containing species detection data
#' @param species_col Character string specifying the column name containing species names
#' @param valid_col Character string specifying the column name containing binary validation data (0/1, TRUE/FALSE)
#' @param conf_col Character string specifying the column name containing confidence scores
#' @param p Numeric value between 0 and 1 specifying the probability threshold (default: 0.95)
#' @param backtransform Logical. If TRUE, uses logit-transformed scores with sensitivity parameter (default: TRUE)
#' @param sensitivity Numeric value for sensitivity parameter used in logit transformation (default: 1.0)
#' @param threshold_only Logical. If TRUE, returns only species and thresholds. If FALSE, returns complete results (default: FALSE)
#' @param plot Logical. If TRUE, creates base R plots for each species (default: FALSE)
#'
#' @return If threshold_only = TRUE, returns a data frame with species and thresholds.
#'         If threshold_only = FALSE, returns a list containing:
#'         \itemize{
#'           \item thresholds: data frame with species and calculated thresholds
#'           \item models: list of fitted glm models for each species (contains raw data)
#'           \item predictions: data frame with predictions and confidence intervals for each species
#'         }
#'
#' @examples
#' # Example with single species
#' df_single <- data.frame(
#'   species = rep("Species_A", 100),
#'   is_valid = rbinom(100, 1, 0.7),
#'   confidence_score = runif(100, 0, 1)
#' )
#'
#' # Get thresholds only (non-transformed)
#' thresholds_only <- species_threshold(df_single, "species", "is_valid", "confidence_score",
#'                                      backtransform = FALSE, threshold_only = TRUE)
#'
#' # Get complete results with logit transformation and plots
#' full_results <- species_threshold(df_single, "species", "is_valid", "confidence_score",
#'                                   backtransform = TRUE, sensitivity = 1.0, plot = TRUE)
#'
#' @export
species_threshold <- function(df,
                              species_col,
                              valid_col,
                              conf_col,
                              p = 0.95,
                              backtransform = TRUE,
                              sensitivity = 1.0,
                              threshold_only = FALSE,
                              plot = FALSE) {

  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  required_cols <- c(species_col, valid_col, conf_col)
  missing_cols <- required_cols[!required_cols %in% names(df)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  if (p <= 0 || p >= 1) {
    stop("p must be between 0 and 1")
  }

  # Convert validation column to numeric (0/1) if it's logical
  df[[valid_col]] <- as.numeric(df[[valid_col]])

  # Get unique species
  species_list <- unique(df[[species_col]])

  # Initialize results storage
  threshold_results <- data.frame(
    species = character(0),
    threshold = numeric(0),
    logit_threshold = numeric(0),
    stringsAsFactors = FALSE
  )

  model_results <- list()
  prediction_results <- data.frame()

  # Process each species
  for (species in species_list) {
    # Subset data for current species
    species_data <- df[df[[species_col]] == species, ]

    # Check if we have variation in the response variable
    if (length(unique(species_data[[valid_col]])) < 2) {
      warning(paste("Species", species, "has no variation in validation data. Skipping."))
      next
    }

    # Check if we have enough data points
    if (nrow(species_data) < 10) {
      warning(paste("Species", species, "has fewer than 10 observations. Results may be unreliable."))
    }

    tryCatch({
      # Prepare data based on backtransform parameter
      if (backtransform) {
        # Apply logit transformation with sensitivity
        species_data$model_score <- log(species_data[[conf_col]] / (1 - species_data[[conf_col]])) / sensitivity
        # Remove infinite values
        species_data <- species_data[is.finite(species_data$model_score), ]

        if (nrow(species_data) == 0) {
          warning(paste("Species", species, "has no valid logit scores after transformation. Skipping."))
          next
        }

        predictor_col <- "model_score"
      } else {
        # Use original confidence scores
        species_data$model_score <- species_data[[conf_col]]
        predictor_col <- "model_score"
      }

      # Fit logistic regression model
      formula_str <- paste(valid_col, "~", predictor_col)
      model <- glm(as.formula(formula_str),
                   family = binomial(),
                   data = species_data)

      # Calculate threshold on model scale
      logit_threshold <- (log(p/(1-p)) - model$coefficients[1]) / model$coefficients[2]

      # Convert threshold back to original confidence score scale
      if (backtransform) {
        # Scale back by sensitivity for original threshold
        logit_threshold_scaled <- logit_threshold * sensitivity
        original_threshold <- exp(logit_threshold_scaled) / (1 + exp(logit_threshold_scaled))
      } else {
        # Threshold is already on original scale
        original_threshold <- logit_threshold
      }

      # Store threshold result
      threshold_results <- rbind(threshold_results,
                                 data.frame(species = species,
                                            threshold = as.numeric(original_threshold),
                                            logit_threshold = as.numeric(logit_threshold),
                                            stringsAsFactors = FALSE))

      # If not threshold_only, calculate predictions and confidence intervals
      if (!threshold_only) {
        # Store model
        model_results[[species]] <- model

        # Create score range for predictions - always cover full 0-1 confidence range
        if (backtransform) {
          # Start with full confidence range (0 to 1)
          score_range_original <- seq(0.0001, 0.9999, length.out = 100)  # Avoid exact 0 and 1 to prevent infinite logits
          # Convert to logit scale for model predictions
          score_range_model <- log(score_range_original / (1 - score_range_original)) / sensitivity
        } else {
          # Use original confidence scale
          score_range_original <- seq(0, 1, length.out = 100)
          score_range_model <- score_range_original
        }

        # Generate predictions with confidence intervals
        pred_data <- data.frame(model_score = score_range_model)
        names(pred_data) <- predictor_col

        pred <- predict(model,
                        newdata = pred_data,
                        type = "link",
                        se.fit = TRUE)

        # Calculate confidence intervals
        z_value <- qnorm(0.975)  # 95% CI
        lower_link <- pred$fit - z_value * pred$se.fit
        upper_link <- pred$fit + z_value * pred$se.fit

        # Transform to probability scale
        species_pred <- data.frame(
          species = species,
          score_original = score_range_original,
          score_model = score_range_model,
          pred_prob = plogis(pred$fit),
          lower_CI = plogis(lower_link),
          upper_CI = plogis(upper_link),
          stringsAsFactors = FALSE
        )

        # Combine prediction results
        prediction_results <- rbind(prediction_results, species_pred)
      }

    }, error = function(e) {
      warning(paste("Error fitting model for species", species, ":", e$message))
    })
  }

  # Create plots if requested
  if (plot && !threshold_only && nrow(threshold_results) > 0) {
    # Create plot for each species
    for (species in threshold_results$species) {
      species_pred <- prediction_results[prediction_results$species == species, ]
      species_threshold <- threshold_results[threshold_results$species == species, "threshold"]
      species_data <- df[df[[species_col]] == species, ]

      # Plot prediction curve on original confidence scale
      plot(species_pred$score_original, species_pred$pred_prob,
           type = "l", col = "blue", lwd = 2,
           xlab = "Confidence Score",
           ylab = "Probability of Correct Detection",
           main = paste("Species:", species,
                        ifelse(backtransform, "(Logit-transformed)", "")),
           ylim = c(0, 1),
           xlim = c(0, 1))

      # Add observed data points on original scale
      points(species_data[[conf_col]], species_data[[valid_col]],
             col = "black", pch = 16)

      # Add confidence intervals
      polygon(c(species_pred$score_original, rev(species_pred$score_original)),
              c(species_pred$lower_CI, rev(species_pred$upper_CI)),
              col = rgb(0, 0, 1, 0.2), border = NA)

      # Add threshold line
      abline(v = species_threshold, col = "red", lty = 2, lwd = 2)

      # Add probability threshold line
      abline(h = p, col = "red", lty = 3, lwd = 1)

      # Add the threshold value
      text(x = species_threshold + 0.01 * diff(par("usr")[1:2]),
           y = p - 0.01 * diff(par("usr")[3:4]),
           labels = sprintf("%.3f", species_threshold),
           col = "red",
           font = 1,
           adj = c(0, 1))

      # Add threshold point
      points(x = species_threshold, y = p, pch = 19, col = "red", cex = 1)

      # Add legend
      legend_text <- c("Prediction", "95% CI",
                       paste("Threshold (", round(species_threshold, 3), ")", sep = ""),
                       paste("P =", p))
      if (backtransform) {
        legend_text <- c(legend_text, paste("Sensitivity =", sensitivity))
      }

      legend("bottomright",
             legend = legend_text,
             col = c("blue", rgb(0, 0, 1, 0.2), "red", "red",
                     if(backtransform) "black" else NULL),
             lty = c(1, 1, 2, 3, if(backtransform) 0 else NULL),
             lwd = c(2, 5, 2, 1, if(backtransform) 0 else NULL),
             pch = c(NA, NA, NA, NA, if(backtransform) NA else NULL),
             cex = 0.8)
    }
  }

  # Return results based on threshold_only parameter
  if (threshold_only) {
    return(threshold_results[, c("species", "threshold")])
  } else {
    return(list(
      thresholds = threshold_results,
      models = model_results,
      predictions = prediction_results,
      parameters = list(p = p, species_col = species_col, valid_col = valid_col,
                        conf_col = conf_col, backtransform = backtransform,
                        sensitivity = sensitivity)
    ))
  }
}
