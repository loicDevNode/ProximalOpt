.validate_lasso_input <- function(X, y, lambda, max_iter, tol) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)
  if (length(dim(X)) != 2L || nrow(X) < 2L || ncol(X) < 1L ||
      any(!is.finite(X))) {
    stop("`X` must be a finite numeric matrix with at least 2 rows and 1 column.",
         call. = FALSE)
  }
  if (length(y) != nrow(X) || any(!is.finite(y))) {
    stop("`y` must be finite and have one value per row of `X`.", call. = FALSE)
  }
  if (length(lambda) != 1L || !is.finite(lambda) || lambda < 0) {
    stop("`lambda` must be one finite non-negative number.", call. = FALSE)
  }
  if (max_iter < 1 || max_iter != as.integer(max_iter)) {
    stop("`max_iter` must be a positive integer.", call. = FALSE)
  }
  if (length(tol) != 1L || !is.finite(tol) || tol <= 0) {
    stop("`tol` must be positive and finite.", call. = FALSE)
  }
  list(X = X, y = y)
}

.solve_lasso <- function(X, y, lambda, max_iter, tol, accelerated) {
  z <- .validate_lasso_input(X, y, lambda, max_iter, tol)
  X <- z$X
  y <- z$y
  n <- nrow(X)
  p <- ncol(X)

  means <- colMeans(X)
  scales <- apply(X, 2L, stats::sd)
  scales[!is.finite(scales) | scales == 0] <- 1
  Xs <- scale(X, center = means, scale = scales)
  y_mean <- mean(y)
  yc <- y - y_mean

  L <- max(eigen(crossprod(Xs) / n, symmetric = TRUE,
                 only.values = TRUE)$values)
  if (!is.finite(L) || L <= 0) {
    stop("The design matrix has no usable variation.", call. = FALSE)
  }
  step <- 1 / L
  beta <- rep(0, p)
  yk <- beta
  momentum <- 1
  converged <- FALSE
  objective_history <- numeric()

  for (k in seq_len(max_iter)) {
    grad <- as.numeric(crossprod(Xs, Xs %*% yk - yc) / n)
    beta_new <- soft_threshold(yk - step * grad, step * lambda)
    residual <- as.numeric(yc - Xs %*% beta_new)
    objective_history[k] <- sum(residual^2) / (2 * n) +
      lambda * sum(abs(beta_new))

    if (sqrt(sum((beta_new - beta)^2)) <=
        tol * max(1, sqrt(sum(beta^2)))) {
      beta <- beta_new
      converged <- TRUE
      break
    }

    if (accelerated) {
      momentum_new <- (1 + sqrt(1 + 4 * momentum^2)) / 2
      yk <- beta_new + ((momentum - 1) / momentum_new) * (beta_new - beta)
      momentum <- momentum_new
    } else {
      yk <- beta_new
    }
    beta <- beta_new
  }

  names(beta) <- colnames(X)
  coef_original <- beta / scales
  names(coef_original) <- colnames(X)
  intercept <- y_mean - sum(means * coef_original)
  fitted <- as.numeric(intercept + X %*% coef_original)

  structure(list(
    coefficients = coef_original,
    standardized_coefficients = beta,
    intercept = intercept,
    fitted.values = fitted,
    lambda = lambda,
    iterations = k,
    converged = converged,
    step_size = step,
    means = means,
    scales = scales,
    objective = objective_history,
    method = if (accelerated) "FISTA" else "ISTA"
  ), class = "proximalopt_lasso")
}

#' Fit an L1-regularized linear model with ISTA
#' @param X Numeric predictor matrix.
#' @param y Numeric response vector.
#' @param lambda Non-negative L1 penalty.
#' @param max_iter Maximum iterations.
#' @param tol Convergence tolerance.
#' @return A fitted `proximalopt_lasso` model.
#' @export
ista <- function(X, y, lambda, max_iter = 5000L, tol = 1e-7) {
  .solve_lasso(X, y, lambda, max_iter, tol, accelerated = FALSE)
}

#' Fit an L1-regularized linear model with FISTA
#' @inheritParams ista
#' @return A fitted `proximalopt_lasso` model.
#' @export
fista <- function(X, y, lambda, max_iter = 5000L, tol = 1e-7) {
  .solve_lasso(X, y, lambda, max_iter, tol, accelerated = TRUE)
}

#' Fit an L1-regularized linear model using proximal gradient
#'
#' This is an alias for the accelerated solver (FISTA).
#' @inheritParams ista
#' @return A fitted `proximalopt_lasso` model.
#' @export
lasso_proximal <- function(X, y, lambda, max_iter = 5000L, tol = 1e-7) {
  fista(X, y, lambda, max_iter, tol)
}

#' Predict from a ProximalOpt LASSO model
#' @param object A fitted ProximalOpt model.
#' @param newdata New predictor matrix or data frame.
#' @param ... Unused.
#' @return Numeric predictions.
#' @export
predict.proximalopt_lasso <- function(object, newdata, ...) {
  newdata <- as.matrix(newdata)
  if (ncol(newdata) != length(object$coefficients)) {
    stop("`newdata` must have the same number of columns as the training matrix.",
         call. = FALSE)
  }
  if (!is.null(names(object$coefficients)) &&
      !is.null(colnames(newdata))) {
    newdata <- newdata[, names(object$coefficients), drop = FALSE]
  }
  as.numeric(object$intercept + newdata %*% object$coefficients)
}
