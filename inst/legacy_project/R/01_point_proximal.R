# ============================================================
# 01_point_proximal.R — Comprendre l'opérateur proximal
# ============================================================

# L'opérateur proximal d'une fonction convexe f est :
# prox_{lambda f}(v) = argmin_x [ f(x) + (1/(2 lambda))*||x-v||^2 ].
# Il réalise un compromis entre minimiser f et rester proche de v.

# 1) Fonction quadratique : f(x) = (1/2)*(x-a)^2
# Son proximal est explicite : prox_{lambda f}(v) = (v + lambda*a)/(1+lambda).
prox_quadratique <- function(v, a, lambda) {
  stopifnot(lambda > 0)
  (v + lambda * a) / (1 + lambda)
}

v <- 5
a <- 1
lambda <- 2
x_prox <- prox_quadratique(v, a, lambda)
cat("Proximal quadratique :\n")
cat("v =", v, "| a =", a, "| lambda =", lambda, "| prox =", x_prox, "\n")
# Résultat : 2.333... ; le point se rapproche de a=1.

# 2) Proximal de la valeur absolue : f(x)=|x|.
# Il s'agit du seuillage doux (soft-thresholding).
soft_threshold <- function(x, lambda) {
  stopifnot(lambda >= 0)
  sign(x) * pmax(abs(x) - lambda, 0)
}

test_values <- c(-3, -1, -0.4, 0, 0.8, 2.5)
cat("\nSeuillage doux avec lambda=1 :\n")
print(data.frame(x=test_values, prox=soft_threshold(test_values, 1)))

# 3) Visualisation : effet de lambda sur une valeur.
lambdas <- seq(0, 3, length.out = 100)
prox_values <- sapply(lambdas, function(l) {
  if (l == 0) v else prox_quadratique(v, a, l)
})
png(file.path("figures", "01_proximal_quadratique.png"),
    width=900, height=600, res=120)
plot(lambdas, prox_values, type="l", lwd=2,
     xlab=expression(lambda), ylab="proximal",
     main="Proximal d'une fonction quadratique")
abline(h=a, lty=2, col="gray40")
dev.off()

cat("\nIntuition : lambda contrôle l'intensité du compromis.\n")
