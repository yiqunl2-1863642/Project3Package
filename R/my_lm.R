#' Linear model function
#'
#' This function fits a linear model to the data given.
#'
#' @param formula String input to be used as the formula for the linear model.
#' @param data Data frame input to be used as the data the linear model used to
#'   fit on.
#' @keywords inference
#'
#' @return a table with rows for each coefficient of the linear model including
#'   the intercept, and columns for \code{Estimate}, \code{Std. Error},
#'   \code{t value}, and \code{Pr(>|t|)}.
#'
#' @examples
#' data(mtcars)
#' my_lm(mpg ~ hp + wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  # calculate the x matrix
  x <- model.matrix(formula, data)
  frame <- model.frame(formula, data)
  # calculate the y matrix
  y <- model.response(frame)
  # calculate the beta matrix
  beta <- solve(t(x) %*% x) %*% t(x) %*% y
  # calculate the degrees of freedom
  df <- nrow(x) - ncol(x)
  # calculate the variance
  my_var <- 0
  for (i in 1:nrow(x)) {
    my_var = my_var + ((y[i] - x[i,] %*% beta)^2 / df)
  }
  # convert the variance from a 1x1 matrix to a number for further calculations
  my_var = my_var[1,1]
  # calculate the standard error
  se <- sqrt(diag(my_var * solve(t(x) %*% x)))
  # calculate the t values for each coefficient
  my_t <- beta / se
  # calculate the area under curve for t distribution
  pr <- 2 * pt(abs(my_t), df, lower.tail = FALSE)
  # generate the result table in data.frame form
  result <- data.frame(
    "Estimate" = beta,
    "Std.Error" = se,
    "t value" = my_t,
    "Pr(>|t|)" = pr,
    check.names = FALSE
  )
  # return the result table
  return(result)
}
