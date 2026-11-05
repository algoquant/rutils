test_that("coordinate descent records each coordinate update", {
  f <- function(x) sum(x^2)
  x0 <- c(2, 3)
  result <- optim_coordescent(f, x0, maxiter = 1)

  expect_equal(result$history, rbind(c(2, 3), c(0, 3), c(0, 0)),
               tolerance = 1e-8)
  expect_equal(result$history[1, ], x0)
  expect_equal(result$history[nrow(result$history), ], result$par)
  expect_equal(result$iter, 1)
})

test_that("coordinate descent does not record skipped coordinates or sweeps", {
  f <- function(x) sum(x^2)
  result <- optim_coordescent(f, c(0, 3, 0), tol = 1e-4)

  expect_equal(result$history, rbind(c(0, 3, 0), c(0, 0, 0)),
               tolerance = 1e-8)
  expect_equal(result$iter, 2)

  optimal <- optim_coordescent(f, c(0, 0))
  expect_equal(optimal$history, matrix(c(0, 0), nrow = 1))
  expect_equal(optimal$iter, 1)
})

test_that("coordinate descent records a converged parabolic path", {
  f <- function(x) {
    (x[1] - 1)^2 + 4 * (x[2] - 2)^2 + 3 * (x[1] - 1) * (x[2] - 2)
  }
  result <- optim_coordescent(f, c(5, 5))
  steps <- result$history[-1, , drop = FALSE] -
    result$history[-nrow(result$history), , drop = FALSE]

  expect_true(all(rowSums(steps != 0) == 1))
  expect_true(all(diff(apply(result$history, 1, f)) <= 1e-12))
  expect_equal(result$par, c(1, 2), tolerance = 1e-5)
  expect_equal(result$history[nrow(result$history), ], result$par)
  expect_lte(nrow(result$history), 2 * result$iter + 1)
})
