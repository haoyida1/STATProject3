#' My t test
#'
#' This function performs a one sample t-test.
#'
#' @param x a numeric vector of data.
#' @param alternative a character string specifying the alternative hypothesis.
#'   This should only accept `"two.sided"`, `"less"`, or `"greater"`.
#'   Otherwise, your function should throw an informative error.
#' @param mu a number indicating the null hypothesis value of the mean.
#'
#' @return A list object containing:
#' * `test_stat`: the numeric test statistic.
#' * `df`: the degrees of freedom.
#' * `alternative`: the value of the parameter `alternative`.
#' * `p_val`: the numeric p-value.
#'
#' @keywords inference
#'
#' @examples
#' data(mtcars)
#' my_t_test(x = mtcars$mpg, alternative = "two.sided",  mu = 20)
#'
#' @export
my_t_test <- function(x, alternative, mu) {
  if (!is.numeric(x)) {
    stop("x must be a numeric vector of data")
  }

  est <- mean(x)
  df <- length(x) - 1
  se <- sd(x)/sqrt(length(x))
  t_obs <- (est-mu)/se

  if (alternative == "less") {
    p_val <- pt(t_obs, df)
  } else if (alternative == "greater") {
    p_val <- pt(t_obs, df, lower.tail = FALSE)
  } else if (alternative == "two.sided") {
    p_val <- 2*pt(abs(t_obs), df, lower.tail = FALSE)
  } else {
    stop("Alternative parameter must be either: 'two.sided', 'less', or 'greater'.")
  }

  result <- list("test_stat" = t_obs,
                 "df" = df,
                 "alternative" = alternative,
                 "p-val" = p_val)
  return(result)
}
