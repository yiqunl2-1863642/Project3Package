% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_lm.R
\name{my_lm}
\alias{my_lm}
\title{Linear model function}
\usage{
my_lm(formula, data)
}
\arguments{
\item{formula}{String input to be used as the formula for the linear model.}

\item{data}{Data frame input to be used as the data the linear model used to
fit on.}
}
\value{
a table with rows for each coefficient of the linear model including
the intercept, and columns for \code{Estimate}, \code{Std. Error},
\code{t value}, and \code{Pr(>|t|)}.
}
\description{
This function fits a linear model to the data given.
}
\examples{
data(mtcars)
my_lm(mpg ~ hp + wt, data = mtcars)

}
\keyword{inference}
