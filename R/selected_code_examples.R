# Selected illustrative R code examples for a time-series forecasting portfolio.
# These are adapted examples based on the workflow described in the team report.
# They are NOT the original team's source files and are NOT a complete pipeline.

# Calculate common forecast metrics.
# The MPE sign convention here is actual - predicted:
# positive values indicate under-prediction on average; negative values indicate over-prediction.
calculate_forecast_metrics <- function(actual, predicted, denominator_floor = 1) {
  if (length(actual) != length(predicted)) {
    stop("actual and predicted must have the same length")
  }
  if (length(actual) == 0) {
    stop("actual and predicted cannot be empty")
  }

  keep <- is.finite(actual) & is.finite(predicted)
  actual <- actual[keep]
  predicted <- predicted[keep]
  if (length(actual) == 0) {
    stop("No finite actual/predicted pairs to evaluate")
  }

  error <- actual - predicted
  safe_denominator <- pmax(abs(actual), denominator_floor)

  data.frame(
    RMSE = sqrt(mean(error^2)),
    MAE = mean(abs(error)),
    MAPE_percent = mean(abs(error) / safe_denominator) * 100,
    MPE_percent = mean(error / safe_denominator) * 100
  )
}

# Generate expanding-window train/test indices for rolling-origin evaluation.
# Each split trains on earlier observations and evaluates the next `horizon` points.
make_rolling_origin_splits <- function(n_observations,
                                       initial_window,
                                       horizon = 6,
                                       step = 1) {
  if (initial_window < 2 || horizon < 1 || step < 1) {
    stop("initial_window, horizon, and step must be positive valid integers")
  }
  if (initial_window + horizon > n_observations) {
    stop("Not enough observations for the requested initial window and horizon")
  }

  train_ends <- seq(
    from = initial_window,
    to = n_observations - horizon,
    by = step
  )

  lapply(train_ends, function(train_end) {
    list(
      train = seq_len(train_end),
      test = seq.int(train_end + 1, train_end + horizon)
    )
  })
}

# Example usage (illustrative only):
# metrics <- calculate_forecast_metrics(actual = c(12, 15, 14),
#                                       predicted = c(11, 16, 13))
# splits <- make_rolling_origin_splits(n_observations = 48,
#                                      initial_window = 24,
#                                      horizon = 6,
#                                      step = 1)
