#' Soft-thresholding operator
#'
#' Computes the proximal operator of the L1 norm, element by element.
#' @param x Numeric vector or matrix.
#' @param lambda Non-negative threshold.
#' @return An object with the same dimensions as `x`.
#' @export
soft_threshold <- function(x, lambda) {
  if (!is.numeric(x) || any(!is.finite(x))) {
    stop("`x` must contain finite numeric values.", call. = FALSE)
  }
  if (length(lambda) != 1L || !is.numeric(lambda) ||
      !is.finite(lambda) || lambda < 0) {
    stop("`lambda` must be one finite non-negative number.", call. = FALSE)
  }
  sign(x) * pmax(abs(x) - lambda, 0)
}

#' Proximal operator of the L1 norm
#' @inheritParams soft_threshold
#' @return The soft-thresholded input.
#' @export
prox_l1 <- function(x, lambda) soft_threshold(x, lambda)

#' Proximal operator of a quadratic function
#'
#' For f(x) = 1/2 * (x - a)^2, returns the proximal operator
#' of `lambda * f` evaluated at `v`.
#' @param v Numeric input.
#' @param a Numeric target.
#' @param lambda Positive step parameter.
#' @return The proximal point.
#' @export
prox_quadratic <- function(v, a, lambda) {
  if (!is.numeric(v) || any(!is.finite(v)) ||
      !is.numeric(a) || any(!is.finite(a))) {
    stop("`v` and `a` must be finite numeric values.", call. = FALSE)
  }
  if (length(lambda) != 1L || !is.numeric(lambda) ||
      !is.finite(lambda) || lambda <= 0) {
    stop("`lambda` must be one positive finite number.", call. = FALSE)
  }
  (v + lambda * a) / (1 + lambda)
}

#' Projection onto a box
#'
#' @param x Numeric vector.
#' @param lower Scalar or vector of lower bounds.
#' @param upper Scalar or vector of upper bounds.
#' @return The projection of `x` onto the box defined by `lower` and `upper`.
#' @export
prox_projection_box <- function(x, lower = -Inf, upper = Inf) {
  if (!is.numeric(x) || anyNA(x)) stop("`x` must be numeric.", call. = FALSE)
  if (any(lower > upper)) stop("Every lower bound must be <= upper.", call. = FALSE)
  pmin(pmax(x, lower), upper)
}
