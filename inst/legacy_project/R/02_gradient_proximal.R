# ============================================================
# 02_gradient_proximal.R — Algorithme de gradient proximal
# ============================================================

# On veut minimiser F(x)=g(x)+h(x), où :
# - g est différentiable (gradient disponible),
# - h est convexe, éventuellement non différentiable.
# Itération : x_(k+1) = prox_{eta h}(x_k - eta*grad(g)(x_k)).
#
# Exemple : F(x)=1/2*(x-3)^2 + lambda*|x|.
# grad(g)(x)=x-3 ; prox_{eta*h} est le seuillage doux.

soft_threshold <- function(x, lambda) sign(x) * pmax(abs(x)-lambda, 0)

gradient_proximal_1d <- function(x0, eta, lambda, max_iter=500, tol=1e-10) {
  stopifnot(eta > 0, lambda >= 0)
  x <- x0
  history <- numeric(max_iter + 1)
  history[1] <- x
  for (k in seq_len(max_iter)) {
    grad_g <- x - 3
    x_new <- soft_threshold(x - eta * grad_g, eta * lambda)
    history[k + 1] <- x_new
    if (abs(x_new - x) < tol) {
      history <- history[seq_len(k + 1)]
      break
    }
    x <- x_new
  }
  list(solution=tail(history, 1), history=history)
}

lambda <- 0.8
res <- gradient_proximal_1d(x0=6, eta=0.5, lambda=lambda)
cat("Solution approchée :", res$solution, "\n")
cat("Solution théorique : max(3-lambda, 0) =", max(3-lambda, 0), "\n")

png(file.path("figures", "02_convergence_1d.png"), width=900, height=600, res=120)
plot(seq_along(res$history)-1, res$history, type="b", pch=19,
     xlab="Itération k", ylab="x_k",
     main="Convergence du gradient proximal en dimension 1")
abline(h=max(3-lambda,0), lty=2, col="gray40")
dev.off()

# Le choix du pas eta influence la stabilité et la vitesse.
