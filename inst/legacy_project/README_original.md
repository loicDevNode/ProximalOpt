# Exposé — Algorithmes proximaux

Projet pédagogique en R, de l'opérateur proximal à une application LASSO sur les prix de logements à Douala.

## Contenu
- `00_setup.R` : configuration.
- `R/01_point_proximal.R` : proximal quadratique et seuillage doux.
- `R/02_gradient_proximal.R` : algorithme de gradient proximal.
- `R/03_lasso.R` : LASSO résolu par seuillage doux.
- `R/04_visualisations.R` : figures pédagogiques.
- `R/05_cas_concret_logements.R` : cas d'étude immobilier.
- `data/05_donnees_logements.csv` : 500 observations synthétiques.
- `report/rapport.Rmd` : trame de rapport.

## Exécution
1. Ouvrir `Exposé_Algorithmes_Proximaux.Rproj` dans RStudio.
2. Exécuter `00_setup.R`.
3. Exécuter les scripts de `R/` dans l'ordre, ou lancer `R/05_cas_concret_logements.R`.
4. Les graphiques sont enregistrés dans `figures/`, les résultats tabulaires dans `results/`.

Les calculs utilisent R de base. `ggplot2` est facultatif.

## Important
Le dataset est artificiel et construit pour expliquer l'algorithme. Il ne constitue ni une enquête immobilière ni une estimation des prix réels à Douala.
