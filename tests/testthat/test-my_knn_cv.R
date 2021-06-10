test_that("Returns a list", {
  train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
  test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
  cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
  x <- my_knn_cv(train, cl, 1, 5)
  expect_type(x, "list")
})
