test_that("Returns a list", {
  x <- rnorm(10, mean = 0, sd = 1)
  y <- my_t.test(x, "two.sided", 0)
  expect_type(y, "list")
  y <- my_t.test(x, mu = -1, alternative = "less")
  expect_type(y, "list")
  y <- my_t.test(x, mu = -1, alternative = "greater")
  expect_type(y, "list")
})

test_that("Throws error for improper alternative hypothesis", {
  x <- rnorm(10, mean = 0, sd = 1)
  expect_error(my_t.test(x, "abc", 0), "Improper alternative")
})
