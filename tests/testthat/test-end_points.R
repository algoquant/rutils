library(rutils)

# define context for tests of function calc_endpoints()
context("test calc_endpoints")

# test the type returned by calc_endpoints()
test_that("calc_endpoints must be numeric", {
  expect_match(typeof(calc_endpoints(env_etf$VTI)), "numeric")
})

# test the value returned by calc_endpoints()
test_that("calc_endpoints value", expect_false(last(calc_endpoints(env_etf$VTI)) > NROW(env_etf$VTI)))
