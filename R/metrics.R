#' Regression evaluation metrics
#'
#' @param actual Observed numeric values.
#' @param predicted Predicted numeric values of the same length.
#' @return A named numeric vector with MAE, RMSE, and R-squared.
#' @export
regression_metrics <- function(actual, predicted) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  if (length(actual) != length(predicted) || length(actual) < 1L ||
      any(!is.finite(actual)) || any(!is.finite(predicted))) {
    stop("`actual` and `predicted` must be finite vectors of equal non-zero length.",
         call. = FALSE)
  }
  sst <- sum((actual - mean(actual))^2)
  c(
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    R_squared = if (sst == 0) NA_real_ else
      1 - sum((actual - predicted)^2) / sst
  )
}
