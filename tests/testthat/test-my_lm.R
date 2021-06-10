test_that("Returns a list(dataframe is of type list in r)", {
  data(mtcars)
  x <- my_lm(mpg ~ hp + wt, data = mtcars)
  expect_type(x, "list")
})
