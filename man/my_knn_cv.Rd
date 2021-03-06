% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_knn_cv.R
\name{my_knn_cv}
\alias{my_knn_cv}
\title{K-Nearest Neighbors Cross-Validation function}
\usage{
my_knn_cv(train, cl, k_nn, k_cv)
}
\arguments{
\item{train}{Data frame input used as the training data.}

\item{cl}{List of true class values of the training data.}

\item{k_nn}{Numeric input used as the number of neighbors.}

\item{k_cv}{Numeric input used as the number of folds.}
}
\value{
a list composed of class and cv_err. class is a vector of predicted
class of all observations. cv_err is the cross-validation
misclassification error.
}
\description{
This function performs a knn cross-validation.
}
\examples{
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
my_knn_cv(train, cl, 1, 5)

}
\keyword{inference}
\keyword{prediction}
