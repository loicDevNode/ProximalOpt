# ============================================================
# 00_setup.R — Configuration du projet
# Projet : Exposé sur les algorithmes proximaux
# ============================================================

# Conseil : ouvrir le fichier .Rproj puis exécuter ce script.
options(stringsAsFactors = FALSE, digits = 4)
set.seed(2026)

# Détecter la racine du projet sans dépendance externe.
# Si le script est lancé depuis RStudio avec le projet ouvert,
# getwd() doit être le dossier Expose_Algorithmes_Proximaux.
root <- getwd()
required_dirs <- c("R", "data", "results", "figures", "report")
for (d in required_dirs) {
  if (!dir.exists(file.path(root, d))) dir.create(file.path(root, d), recursive = TRUE)
}

cat("Projet :", normalizePath(root), "\n")
cat("R version :", R.version.string, "\n")
cat("Dossiers vérifiés/créés :", paste(required_dirs, collapse = ", "), "\n")

# Dépendances facultatives : les scripts de calcul utilisent uniquement R de base.
# ggplot2 est utilisé pour des graphiques plus soignés si disponible.
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("ggplot2 n'est pas installé. Les scripts peuvent fonctionner avec les graphiques base R.")
} else {
  message("ggplot2 disponible : ", as.character(packageVersion("ggplot2")))
}
