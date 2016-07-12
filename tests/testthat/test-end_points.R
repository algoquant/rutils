library(rutils)

# define context for tests of function end_points()
context("test end_points")

# test the type returned by end_points()
test_that("end_points must be integer", {
  expect_match(typeof(end_points(env_etf$VTI)), "integer")
})

# test the value returned by end_points()
test_that("end_points value", expect_false(last(end_points(env_etf$VTI)) > NROW(env_etf$VTI)))
