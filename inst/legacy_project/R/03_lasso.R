# ============================================================
# 03_lasso.R — Régression LASSO par gradient proximal
# ============================================================

# LASSO : minimiser (1/(2n))*||y-X beta||² + lambda*||beta||_1.
# Gradient de la partie lisse : X'*(X beta-y)/n.
# Proximal de lambda*||beta||_1 : seuillage doux coefficient par coefficient.
# Pas eta <= 1/L, avec L = ||X'X/n||_2.

soft_threshold <- function(x, threshold) {
  sign(x) * pmax(abs(x) - threshold, 0)
}

lasso_proximal <- function(X, y, lambda, max_iter=5000, tol=1e-7) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)
  stopifnot(length(y)==n, lambda>=0)

  # Centrer et réduire les prédicteurs pour comparer les coefficients.
  mu <- colMeans(X)
  sigma <- apply(X, 2, sd)
  sigma[sigma == 0] <- 1
  Xs <- scale(X, center=mu, scale=sigma)
  y_mean <- mean(y)
  yc <- y - y_mean

  # Constante de Lipschitz et pas conservateur.
  L <- max(eigen(crossprod(Xs)/n, symmetric=TRUE, only.values=TRUE)$values)
  eta <- 1 / L

  beta <- rep(0, p)
  for (k in seq_len(max_iter)) {
    grad <- as.numeric(crossprod(Xs, Xs %*% beta - yc) / n)
    beta_new <- soft_threshold(beta - eta*grad, eta*lambda)
    if (sqrt(sum((beta_new-beta)^2)) < tol) {
      beta <- beta_new
      break
    }
    beta <- beta_new
  }

  names(beta) <- colnames(X)
  # Coefficients sur l'échelle standardisée; intercept adapté à X original.
  intercept <- y_mean - sum((mu/sigma) * beta)
  list(coefficients=beta, intercept=intercept, iterations=k,
       lambda=lambda, eta=eta, fitted=as.numeric(intercept + X %*% (beta/sigma)),
       means=mu, scales=sigma)
}

# Démonstration synthétique avec 4 variables et 2 effets réellement forts.
set.seed(2026)
n <- 180
X_demo <- matrix(rnorm(n*4), nrow=n, dimnames=list(NULL, c("x1","x2","x3","x4")))
y_demo <- 2 + 3*X_demo[,1] - 1.5*X_demo[,3] + rnorm(n, sd=0.7)
fit <- lasso_proximal(X_demo, y_demo, lambda=0.25)
cat("Coefficients LASSO (échelle standardisée) :\n")
print(fit$coefficients)
cat("Nombre d'itérations :", fit$iterations, "\n")
