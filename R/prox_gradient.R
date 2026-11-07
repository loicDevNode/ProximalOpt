#' Proximal gradient algorithm
#'
#' Minimizes a composite objective F(x) = g(x) + h(x), given the gradient
#' of the smooth part and the proximal operator of the non-smooth part.
#' @param x0 Initial numeric vector.
#' @param grad Function returning the gradient of g at x.
#' @param prox Function applying the proximal operator with parameter
#' `step * h` to its first argument.
#' @param step_size Positive gradient step size.
#' @param max_iter Maximum number of iterations.
#' @param tol Stopping tolerance based on successive iterates.
#' @param objective Optional function returning F(x), used for diagnostics.
#' @return A list containing the solution, iteration history, convergence
#'   status, iteration count, and optional objective history.
#' @export
prox_gradient <- function(x0, grad, prox, step_size,
                          max_iter = 1000L, tol = 1e-8,
                          objective = NULL) {
  if (!is.numeric(x0) || length(x0) < 1L || any(!is.finite(x0))) {
    stop("`x0` must be a non-empty finite numeric vector.", call. = FALSE)
  }
  if (!is.function(grad) || !is.function(prox)) {
    stop("`grad` and `prox` must be functions.", call. = FALSE)
  }
  if (length(step_size) != 1L || !is.finite(step_size) || step_size <= 0) {
    stop("`step_size` must be positive and finite.", call. = FALSE)
  }
  if (length(max_iter) != 1L || max_iter < 1 || max_iter != as.integer(max_iter)) {
    stop("`max_iter` must be a positive integer.", call. = FALSE)
  }
  if (length(tol) != 1L || !is.finite(tol) || tol <= 0) {
    stop("`tol` must be positive and finite.", call. = FALSE)
  }

  x <- as.numeric(x0)
  history <- matrix(NA_real_, nrow = max_iter + 1L, ncol = length(x))
  history[1L, ] <- x
  objective_history <- if (is.function(objective)) {
    c(objective(x))
  } else {
    NULL
  }
  converged <- FALSE

  for (k in seq_len(max_iter)) {
    x_new <- prox(x - step_size * grad(x), step_size)
    if (!is.numeric(x_new) || length(x_new) != length(x) ||
        any(!is.finite(x_new))) {
      stop("`prox` must return a finite numeric vector matching `x0`.", call. = FALSE)
    }
    history[k + 1L, ] <- x_new
    if (!is.null(objective_history)) {
      objective_history <- c(objective_history, objective(x_new))
    }
    if (sqrt(sum((x_new - x)^2)) <= tol * max(1, sqrt(sum(x^2)))) {
      x <- x_new
      converged <- TRUE
      break
    }
    x <- x_new
  }

  used <- k + 1L
  history <- history[seq_len(used), , drop = FALSE]
  colnames(history) <- names(x0)
  list(
    solution = x,
    history = history,
    objective = objective_history,
    iterations = used - 1L,
    converged = converged
  )
}

#' @rdname prox_gradient
#' @export
proximal_gradient <- prox_gradient
