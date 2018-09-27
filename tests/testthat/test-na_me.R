library(rutils)

# define context for tests of function get_name()
context("test get_name")

test_that("type must be character", {
  expect_match(typeof(get_name(colnames(etf_env$VTI)[1])), "character")
})

test_that("type must be character", {
  mat_rix <- matrix(1:6, nrow=1, dimnames=list("row1", paste0("mat_rix.col", 1:6)))
  x_ts <- xts::xts(mat_rix, order.by=Sys.Date())
  expect_match(typeof(get_name(colnames(mat_rix)[1])), "character")
  expect_match(typeof(get_name(colnames(etf_env$VTI)[1])), "character")
})

test_that("value must be VTI", {
  expect_match(get_name(colnames(etf_env$VTI)[1]), "VTI")
})

