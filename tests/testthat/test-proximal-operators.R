test_that("soft thresholding gives the expected values", {
  expect_equal(soft_threshold(c(-3, -1, 0, 2), 1), c(-2, 0, 0, 1))
})

test_that("quadratic proximal moves toward the target", {
  expect_equal(prox_quadratic(5, 1, 2), (5 + 2) / 3)
})

test_that("box projection respects its bounds", {
  expect_equal(prox_projection_box(c(-2, 0.5, 5), 0, 1), c(0, 0.5, 1))
})

test_that("negative threshold is rejected", {
  expect_error(soft_threshold(1, -1))
})
