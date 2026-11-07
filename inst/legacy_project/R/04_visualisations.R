# ============================================================
# 04_visualisations.R — Visualiser le seuillage et la sparsité
# ============================================================

soft_threshold <- function(x, lambda) sign(x) * pmax(abs(x)-lambda, 0)

# Figure 1 : courbe de seuillage doux.
x <- seq(-4, 4, length.out=401)
lambda <- 1
png(file.path("figures", "04_seuillage_doux.png"), width=900, height=600, res=120)
plot(x, soft_threshold(x, lambda), type="l", lwd=2,
     xlab="Valeur initiale x", ylab="prox_{lambda |.|}(x)",
     main="Opérateur proximal de la valeur absolue")
abline(a=0,b=1,lty=2,col="gray50")
abline(v=c(-lambda,lambda),lty=3,col="gray50")
dev.off()

# Figure 2 : chemin des coefficients LASSO selon lambda.
set.seed(44)
n <- 160
X <- matrix(rnorm(n*5), n, 5)
colnames(X) <- paste0("x",1:5)
y <- 1.7*X[,1] - 2.4*X[,3] + rnorm(n,sd=0.8)
lambdas <- seq(0.02, 1.5, length.out=50)

# Ici, recalcul indépendant pour chaque lambda afin d'illustrer la décroissance.
source(file.path("R","03_lasso.R"), local=TRUE)
coef_path <- sapply(lambdas, function(lam) lasso_proximal(X,y,lambda=lam)$coefficients)

png(file.path("figures", "04_chemin_coefficients.png"), width=900, height=600, res=120)
matplot(lambdas, t(coef_path), type="l", lty=1, lwd=2,
        xlab="Lambda", ylab="Coefficient standardisé",
        main="Chemin des coefficients LASSO")
abline(h=0,lty=2,col="gray50")
legend("topright", legend=colnames(X), col=seq_len(ncol(X)), lty=1, cex=0.8)
dev.off()
