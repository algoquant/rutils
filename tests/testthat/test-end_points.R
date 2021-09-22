library(rutils)

# define context for tests of function calc_endpoints()
context("test calc_endpoints")

# test the type returned by calc_endpoints()
test_that("calc_endpoints must be numeric", {
  expect_match(class(calc_endpoints(1:100, 11)), "numeric")
})

# test the value returned by calc_endpoints()
test_that("calc_endpoints value", expect_false(last(calc_endpoints(1:100, 11)) > NROW(1:100)))
