# ============================================================
# 05_cas_concret_logements.R — Application au prix des logements
# Douala, Cameroun — données synthétiques pour démonstration
# ============================================================

source("00_setup.R")
source(file.path("R", "03_lasso.R"))

data_path <- file.path("data", "05_donnees_logements.csv")
if (!file.exists(data_path)) stop("Fichier de données absent : ", data_path)
housing <- read.csv(data_path)

cat("\nAperçu des données :\n")
print(head(housing))
cat("\nRépartition par zone :\n")
print(table(housing$zone))
cat("\nRésumé des prix (millions FCFA) :\n")
print(summary(housing$prix_millions_fcfa))

# Construire les variables explicatives : indicatrices de zones + variables numériques.
housing$zone <- factor(housing$zone)
X <- model.matrix(
  ~ superficie_m2 + nombre_chambres + age_logement_ans +
    distance_centre_km + zone,
  data=housing
)[, -1, drop=FALSE]
y <- housing$prix_millions_fcfa

# Séparation entraînement/test reproductible.
set.seed(2026)
train_id <- sample(seq_len(nrow(housing)), size=floor(0.8*nrow(housing)))
test_id <- setdiff(seq_len(nrow(housing)), train_id)

# Lambda de démonstration. En pratique, on choisit lambda par validation croisée.
lambda <- 1.2
model <- lasso_proximal(X[train_id,,drop=FALSE], y[train_id], lambda=lambda)

pred <- as.numeric(model$intercept + X[test_id,,drop=FALSE] %*%
                     (model$coefficients / model$scales))
actual <- y[test_id]
mae <- mean(abs(actual-pred))
rmse <- sqrt(mean((actual-pred)^2))
r2 <- 1 - sum((actual-pred)^2)/sum((actual-mean(actual))^2)

cat("\nÉvaluation sur le jeu test :\n")
cat("MAE  :", round(mae, 2), "millions FCFA\n")
cat("RMSE :", round(rmse, 2), "millions FCFA\n")
cat("R²   :", round(r2, 3), "\n")

# Exporter prédictions et coefficients.
predictions <- data.frame(
  id_logement=housing$id_logement[test_id],
  zone=housing$zone[test_id],
  prix_reel_millions_fcfa=actual,
  prix_predit_millions_fcfa=round(pred,2),
  erreur_millions_fcfa=round(actual-pred,2)
)
write.csv(predictions, file.path("results","05_predictions_logements.csv"), row.names=FALSE)

coefs <- data.frame(
  variable=names(model$coefficients),
  coefficient_standardise=as.numeric(model$coefficients),
  coefficient_unite_originale=as.numeric(model$coefficients/model$scales)
)
write.csv(coefs, file.path("results","05_coefficients_logements.csv"), row.names=FALSE)

# Graphique réel vs prédit.
png(file.path("figures","05_predictions.png"), width=900, height=650, res=120)
plot(actual, pred, pch=19, xlab="Prix réel (millions FCFA)",
     ylab="Prix prédit (millions FCFA)",
     main="Prix réels et prix prédits — logements à Douala")
abline(0,1,lty=2,lwd=2,col="gray40")
dev.off()

# Graphique des coefficients (sur échelle standardisée).
png(file.path("figures","05_coefficients.png"), width=1000, height=650, res=120)
barplot(model$coefficients, las=2, cex.names=0.75,
        ylab="Coefficient standardisé",
        main="Coefficients LASSO — modèle immobilier")
abline(h=0,lty=2,col="gray40")
dev.off()

# Courbe de convergence de l'objectif pour visualiser le déroulement de l'optimisation.
# Fonction objectif évaluée avec les coefficients finaux sur le train.
Xtr <- scale(X[train_id,,drop=FALSE], center=model$means, scale=model$scales)
ytr <- y[train_id] - mean(y[train_id])
beta <- model$coefficients
obj <- sum((ytr - Xtr %*% beta)^2)/(2*length(train_id)) + lambda*sum(abs(beta))
cat("\nValeur finale de l'objectif LASSO :", round(obj,4), "\n")

cat("\nInterprétation :\n")
cat("- Les coefficients non nuls signalent des variables retenues par le LASSO.\n")
cat("- Les coefficients nuls correspondent à des variables écartées pour ce lambda.\n")
cat("- Les données sont simulées : les résultats illustrent la méthode, pas les prix du marché.\n")
