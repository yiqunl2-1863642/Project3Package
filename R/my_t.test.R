#' T-test function
#'
#' This function performs a one sample t-test.
#'
#' @param x Numeric vector input to be used as the data for the t-test.
#' @param alternative String input that indicates the alternative hypothesis,
#'   which must be one of  \code{"two.sided"}, \code{"greater"}, or
#'   \code{"less"}.
#' @param mu Numeric input to be used as the true value of mean.
#' @keywords inference
#'
#' @return a list having the following elements, \code{test_stat} the numeric
#'   test statistic, \code{df} the degrees of freedom, \code{alternative} the
#'   string indicating the alternative hypothesis, \code{p_val} the numeric
#'   p-value.
#'
#' @examples
#' x <- rnorm(10, mean = 0, sd = 1)
#' my_t.test(x, "two.sided", 0)
#' my_t.test(x, mu = -1, alternative = "less")
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # calculate T
  my_t <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))
  # calculate the degrees of freedom
  df <- length(x) - 1
  # if alternative hypothesis is mean(x) < mu
  if (alternative == "greater") {
    # calculate the numeric p value
    my_pval <- pt(my_t, df, lower.tail = FALSE)
    # if alternative hypothesis is mean(x) = mu
  } else if (alternative == "two.sided") {
    my_pval <- 2 * pt(abs(my_t), df, lower.tail = FALSE)
    # if alternative hypothesis is mean(x) > mu
  } else if (alternative == "less") {
    my_pval <- pt(my_t, df, lower.tail = TRUE)
    # the altervative argument does not satisfy the requirement
  } else {
    stop("Alternative must be 'two.sided', 'less', or 'greater")
  }
  # return the resultant list
  return(list(
    "test_stat" = my_t,
    "df" = df,
    "alternative" = alternative,
    "p_val" = my_pval))
}
