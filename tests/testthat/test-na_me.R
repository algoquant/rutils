library(rutils)

# define context for tests of function na_me()
context("test na_me")

test_that("type must be character", {
  expect_match(typeof(na_me(env_etf$VTI)), "character")
})

test_that("type must be character", {
  mat_rix <- matrix(1:6, nrow=1, dimnames=list("row1", paste0("mat_rix.col", 1:6)))
  x_ts <- xts::xts(mat_rix, order.by=Sys.Date())
  expect_match(typeof(na_me(mat_rix)), "character")
  expect_match(typeof(na_me(env_etf$VTI)), "character")
})

test_that("value must be VTI", {
  expect_match(na_me(env_etf$VTI), "VTI")
})

