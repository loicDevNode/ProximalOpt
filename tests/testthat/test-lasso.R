test_that("FISTA recovers a sparse linear signal approximately", {
  set.seed(2026)
  X <- matrix(rnorm(400), nrow = 100, ncol = 4)
  colnames(X) <- paste0("x", 1:4)
  y <- 1 + 2 * X[, 1] - 1.5 * X[, 3] + rnorm(100, sd = 0.15)
  fit <- fista(X, y, lambda = 0.03)
  expect_true(is.finite(fit$intercept))
  expect_equal(length(fit$coefficients), 4)
  expect_equal(length(predict(fit, X)), 100)
  expect_lt(mean((y - predict(fit, X))^2), 0.2)
})

test_that("housing data loader returns 500 rows", {
  d <- load_housing_data()
  expect_equal(nrow(d), 500L)
  expect_true(all(c("zone", "superficie_m2", "prix_millions_fcfa") %in% names(d)))
})
