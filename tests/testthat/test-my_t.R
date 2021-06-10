test_that("Returns a list", {
  x <- rnorm(10, mean = 0, sd = 1)
  y <- my_t.test(x, "two.sided", 0)
  expect_type(y, "list")
})
