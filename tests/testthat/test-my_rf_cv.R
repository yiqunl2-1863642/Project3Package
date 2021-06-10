test_that("Returns a float number in type double", {
  x <- my_rf_cv(5)
  expect_type(x, "double")
})
