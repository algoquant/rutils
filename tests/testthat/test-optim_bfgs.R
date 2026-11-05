test_that("BFGS history includes the start and all accepted steps", {
  f <- function(x) {
    (x[1] - 1)^2 + 4 * (x[2] - 2)^2 + 3 * (x[1] - 1) * (x[2] - 2)
  }
  x0 <- c(-3, 4)
  result <- optim_bfgs(f, x0)

  expect_equal(result$history[1, ], x0)
  expect_equal(nrow(result$history), result$iter + 1)
  expect_equal(result$history[nrow(result$history), ], result$par)
  expect_equal(result$par, c(1, 2), tolerance = 1e-6)
  expect_true(all(diff(apply(result$history, 1, f)) <= 0))

  limited <- optim_bfgs(f, x0, maxiter = 1)
  expect_equal(limited$iter, 1)
  expect_equal(limited$history, unname(rbind(x0, limited$par)))
})

test_that("BFGS history contains only the start when already optimal", {
  result <- optim_bfgs(function(x) sum(x^2), c(0, 0))

  expect_equal(result$iter, 0)
  expect_equal(result$history, matrix(c(0, 0), nrow = 1))
  expect_equal(result$history[1, ], result$par)
})

test_that("BFGS history includes a final small backtracking step", {
  f <- function(x) 2 * sum(x^2)
  x0 <- 1
  result <- optim_bfgs(f, x0, tol = 2)

  expect_equal(result$iter, 1)
  expect_equal(result$history, unname(rbind(x0, result$par)))
  expect_true(abs(result$par - x0) < 2)
})
