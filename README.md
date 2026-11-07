# ProximalOpt

**ProximalOpt** is an educational R package for proximal optimization and
sparse statistical learning. It brings together proximal operators, proximal
gradient methods, ISTA, FISTA, LASSO regression, diagnostics, and a synthetic
housing case study inspired by Douala, Cameroon.

> **Data notice:** the 500 housing observations included in this project are
> synthetic. They are not verified real-estate transactions and must not be
> interpreted as actual market prices in Douala.

## Installation

```r
# install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/ProximalOpt")
```

## Quick start

```r
library(ProximalOpt)

soft_threshold(c(-3, -1, 0, 2), lambda = 1)

housing <- load_housing_data()
X <- model.matrix(
  ~ superficie_m2 + nombre_chambres + age_logement_ans +
    distance_centre_km + zone,
  data = housing
)[, -1, drop = FALSE]
y <- housing$prix_millions_fcfa

set.seed(2026)
train <- sample(seq_len(nrow(housing)), 400)
fit <- fista(X[train, , drop = FALSE], y[train], lambda = 1.2)
pred <- predict(fit, X[-train, , drop = FALSE])
regression_metrics(y[-train], pred)
```

## Main modules

1. Proximal operators
2. Proximal gradient
3. ISTA and FISTA
4. LASSO regression
5. Projection onto box constraints
6. Douala housing case study
7. Convergence and coefficient visualizations

## Preserved original project

The complete source files recovered from the earlier **Exposé_Algorithmes_Proximaux**
project are preserved under `inst/legacy_project/`. They include the setup
script, the five numbered R scripts, the original 500-row CSV dataset,
the original README, and the R Markdown report.

To reproduce the original scripts, open the `inst/legacy_project/` folder as
the working directory and run `00_setup.R`, then the scripts in `R/`.

## Development

```r
install.packages(c("devtools", "usethis", "roxygen2", "testthat"))
devtools::document()
devtools::test()
devtools::check()
```

## Roadmap

- Improve mathematical documentation and convergence diagnostics.
- Add a broader collection of proximal operators.
- Add systematic lambda selection and cross-validation.
- Expand the housing case study and compare ISTA/FISTA.
- Prepare a CRAN submission after all checks and policy requirements pass.

## License

MIT. See `LICENSE`.
