#' Plot an optimization objective history
#' @param fit Result from [prox_gradient()], [ista()], or [fista()].
#' @param ... Additional graphical parameters passed to [graphics::plot()].
#' @return Invisibly returns the supplied fit.
#' @export
plot_convergence <- function(fit, ...) {
  values <- fit$objective
  if (is.null(values) || !length(values)) {
    stop("This fit does not contain an objective history.", call. = FALSE)
  }
  graphics::plot(seq_along(values), values, type = "l",
                 xlab = "Iteration", ylab = "Objective value", ...)
  invisible(fit)
}

#' Plot fitted LASSO coefficients
#' @param fit A fitted ProximalOpt LASSO model.
#' @param ... Additional graphical parameters passed to [graphics::barplot()].
#' @return Invisibly returns the coefficient vector.
#' @export
plot_lasso_coefficients <- function(fit, ...) {
  if (is.null(fit$coefficients)) stop("`fit` is not a LASSO model.")
  graphics::barplot(fit$coefficients, las = 2,
                    ylab = "Coefficient", ...)
  graphics::abline(h = 0, lty = 2)
  invisible(fit$coefficients)
}
