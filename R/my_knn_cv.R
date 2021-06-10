#' K-Nearest Neighbors Cross-Validation function
#'
#' This function performs a knn cross-validation.
#'
#' @param train Data frame input used as the training data.
#' @param cl List of true class values of the training data.
#' @param k_nn Numeric input used as the number of neighbors.
#' @param k_cv Numeric input used as the number of folds.
#' @keywords prediction
#'
#' @return a list composed of class and cv_err. class is a vector of predicted
#'   class of all observations. cv_err is the cross-validation
#'   misclassification error.
#'
#' @examples
#' train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
#' test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
#' cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
#' my_knn_cv(train, cl, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # create a vector that randomly assign observations to fold 1,...,k_cv
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  misclassification <- rep(0, k_cv)
  for (i in 1:k_cv) {
    # assign observations and their corresponding class to training data, test
    # data, training class, and test class based on whether their fold is equal
    # to i
    training <- train[fold!=i,]
    testing <- train[fold==i,]
    cl_train <- cl[fold!=i]
    cl_test <- cl[fold==i]
    # using k nearest neighbor to predict the class of test data
    predict <- class::knn(training, testing, cl_train, k=k_nn)
    # record the error rate
    misclassification[i] <- mean(predict != cl_test)
  }
  cla <- class::knn(train, train, cl, k=k_nn)
  cv_error <- mean(misclassification)
  return(list(cla, cv_error))
}
