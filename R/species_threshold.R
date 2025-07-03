#' Calculate confidence threshold for a single species
#'
#' This function fits a logistic regression model to calculate confidence threshold
#' for a single species. It returns either just the threshold or complete model
#' results with predictions.
#'
#' @param df A data frame containing species detection data for a single species
#' @param valid_col Character string specifying the column name containing binary validation data (0/1, TRUE/FALSE)
#' @param conf_col Character string specifying the column name containing confidence scores
#' @param p Numeric value between 0 and 1 specifying the probability threshold (default: 0.95)
#' @param backtransform Logical. If TRUE, uses logit-transformed scores with sensitivity parameter (default: TRUE)
#' @param sensitivity Numeric value for sensitivity parameter used in logit transformation (default: 1.0)
#' @param threshold_only Logical. If TRUE, returns only the threshold value. If FALSE, returns complete results (default: FALSE)
#' @param plot Logical. If TRUE, creates a base R plot (default: FALSE)
#' @param plot_logit Logical. If TRUE and backtransform=TRUE, plots on logit scale instead of original confidence scale (default: FALSE)
#'
#' @return If threshold_only = TRUE, returns a numeric threshold value.
#'         If threshold_only = FALSE, returns a list containing:
#'         \itemize{
#'           \item threshold: numeric threshold value
#'           \item logit_threshold: threshold on the logit scale
#'           \item model: fitted glm model object
#'           \item predictions: data frame with predictions and confidence intervals
#'           \item parameters: list of function parameters used
#'         }
#'
#' @examples
#' # Example with single species data
#' df_single <- data.frame(
#'   is_valid = rbinom(100, 1, 0.7),
#'   confidence_score = runif(100, 0, 1)
#' )
#'
#' # Get threshold only (non-transformed)
#' threshold_only <- species_threshold_single(df_single, "is_valid", "confidence_score",
#'                                           backtransform = FALSE, threshold_only = TRUE)
#'
#' # Get complete results with logit transformation and plot on logit scale
#' full_results <- species_threshold_single(df_single, "is_valid", "confidence_score",
#'                                         backtransform = TRUE, sensitivity = 1.0,
#'                                         plot = TRUE, plot_logit = TRUE)
#'
#' @export
species_threshold <- function(df,
                              valid_col,
                              conf_col,
                              p = 0.95,
                              backtransform = TRUE,
                              sensitivity = 1.0,
                              threshold_only = FALSE,
                              plot = FALSE,
                              plot_logit = FALSE) {

  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  required_cols <- c(valid_col, conf_col)
  missing_cols <- required_cols[!required_cols %in% names(df)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  if (p <= 0 || p >= 1) {
    stop("p must be between 0 and 1")
  }

  if (nrow(df) == 0) {
    stop("Data frame is empty")
  }

  # Validate plot_logit parameter
  if (plot_logit && !backtransform) {
    warning("plot_logit=TRUE requires backtransform=TRUE. Setting plot_logit=FALSE.")
    plot_logit <- FALSE
  }

  # Convert validation column to numeric (0/1) if it's logical
  df[[valid_col]] <- as.numeric(df[[valid_col]])

  # Check if we have variation in the response variable
  if (length(unique(df[[valid_col]])) < 2) {
    stop("No variation in validation data - all values are the same")
  }

  # Check if we have enough data points
  if (nrow(df) < 10) {
    warning("Fewer than 10 observations. Results may be unreliable.")
  }

  # Prepare data based on backtransform parameter
  if (backtransform) {
    # Check for boundary values that would cause infinite logits
    if (any(df[[conf_col]] <= 0) || any(df[[conf_col]] >= 1)) {
      stop("Confidence scores must be between 0 and 1 (exclusive) for logit transformation")
    }

    # Apply logit transformation with sensitivity
    df$transformed_scores <- log(df[[conf_col]] / (1 - df[[conf_col]])) / sensitivity

    # Remove infinite values (shouldn't happen after boundary check, but just in case)
    df <- df[is.finite(df$transformed_scores), ]

    if (nrow(df) == 0) {
      stop("No valid logit scores after transformation")
    }

    predictor_col <- "transformed_scores"
  } else {
    # Use original confidence scores directly
    predictor_col <- conf_col
  }

  # Fit logistic regression model
  formula_str <- paste(valid_col, "~", predictor_col)
  model <- glm(as.formula(formula_str),
               family = binomial(),
               data = df)

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

  # If threshold_only, return just the threshold value
  if (threshold_only) {
    return(as.numeric(original_threshold))
  }

  # Calculate predictions and confidence intervals
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
  pred_data <- data.frame(x = score_range_model)
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
  if (backtransform) {
    # Include both original and transformed scores
    predictions <- data.frame(
      score_original = score_range_original,
      score_transformed = score_range_model,
      pred_prob = plogis(pred$fit),
      lower_CI = plogis(lower_link),
      upper_CI = plogis(upper_link),
      stringsAsFactors = FALSE
    )
  } else {
    # Only include original scores (no transformation)
    predictions <- data.frame(
      score_original = score_range_original,
      pred_prob = plogis(pred$fit),
      lower_CI = plogis(lower_link),
      upper_CI = plogis(upper_link),
      stringsAsFactors = FALSE
    )
  }

  # Create plot if requested
  if (plot) {
    # Determine which scale to use for plotting
    if (plot_logit && backtransform) {
      # Plot on logit scale
      x_values <- predictions$score_transformed
      x_data <- df$transformed_scores
      x_threshold <- logit_threshold
      x_label <- "Logit-transformed Confidence Score"
      x_limits <- range(x_data, na.rm = TRUE)

      # Extend limits slightly for better visualization
      x_range <- diff(x_limits)
      x_limits <- c(x_limits[1] - 0.1 * x_range, x_limits[2] + 0.1 * x_range)

      plot_title <- "Species Threshold Analysis (Logit Scale)"

      # Format threshold label for logit scale
      threshold_label <- sprintf("%.3f", x_threshold)

    } else {
      # Plot on original confidence scale
      x_values <- predictions$score_original
      x_data <- df[[conf_col]]
      x_threshold <- original_threshold
      x_label <- "Confidence Score"
      x_limits <- c(0, 1)

      plot_title <- paste("Species Threshold Analysis",
                          ifelse(backtransform, "(Original Scale)", ""))

      # Format threshold label for original scale
      threshold_label <- sprintf("%.3f", x_threshold)
    }

    # Create the plot
    plot(x_values, predictions$pred_prob,
         type = "l", col = "blue", lwd = 2,
         xlab = x_label,
         ylab = "Probability of True Positive",
         main = plot_title,
         ylim = c(0, 1),
         xlim = x_limits)

    # Add observed data points
    points(x_data, df[[valid_col]],
           col = "black", pch = 16)

    # Add confidence intervals
    polygon(c(x_values, rev(x_values)),
            c(predictions$lower_CI, rev(predictions$upper_CI)),
            col = rgb(0, 0, 1, 0.2), border = NA)

    # Add threshold line
    abline(v = x_threshold, col = "red", lty = 2, lwd = 2)

    # Add probability threshold line
    abline(h = p, col = "red", lty = 3, lwd = 1)

    # Add the threshold value label
    text(x = x_threshold + 0.01 * diff(par("usr")[1:2]),
         y = p - 0.01 * diff(par("usr")[3:4]),
         labels = threshold_label,
         col = "red",
         font = 1,
         adj = c(0, 1))

    # Add threshold point
    points(x = x_threshold, y = p, pch = 19, col = "red", cex = 1)

    # Add legend
    legend_text <- c("Prediction", "95% CI",
                     paste("Threshold (", threshold_label, ")", sep = ""),
                     paste("P =", p))

    if (backtransform) {
      legend_text <- c(legend_text, paste("Sensitivity =", sensitivity))
      if (plot_logit) {
        legend_text <- c(legend_text, "Logit scale")
      }
    }

    legend("bottomright",
           legend = legend_text,
           col = c("blue", rgb(0, 0, 1, 0.2), "red", "red",
                   if(backtransform) "black" else NULL,
                   if(plot_logit && backtransform) "black" else NULL),
           lty = c(1, 1, 2, 3,
                   if(backtransform) 0 else NULL,
                   if(plot_logit && backtransform) 0 else NULL),
           lwd = c(2, 5, 2, 1,
                   if(backtransform) 0 else NULL,
                   if(plot_logit && backtransform) 0 else NULL),
           pch = c(NA, NA, NA, NA,
                   if(backtransform) NA else NULL,
                   if(plot_logit && backtransform) NA else NULL),
           cex = 0.8)
  }

  # Return complete results
  return(list(
    threshold = as.numeric(original_threshold),
    logit_threshold = as.numeric(logit_threshold),
    model = model,
    predictions = predictions,
    parameters = list(p = p, valid_col = valid_col, conf_col = conf_col,
                      backtransform = backtransform, sensitivity = sensitivity,
                      plot_logit = plot_logit)
  ))
}
