#' My Linear Model
#'
#' This function fits a linear model.
#'
#' @param formula a formula class object, similar to \code{lm()}.
#' @param data Data frame input to be fitted.
#'
#' @returns a table similar to the coefficent table from \code{summary()} with rows
#'   for each coefficient and columns for the \code{Estimate}, \code{Std. Error},
#'   \code{t value}, and \code{Pr(>|t|)}. There should be row and column names.
#'
#' @keywords prediction
#'
#' @examples
#' my_lm(formula = mtcars$mpg ~ mtcars$cyl + mtcars$wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  X <- model.matrix(formula, data)
  Y <- model.response(model.frame(formula, data))
  B <- solve(t(X) %*% X) %*% t(X) %*% Y
  A <- (Y - X %*% B)^2

  df <- nrow(X) - ncol(X)
  sig_sqr <- sum((Y - X %*% B)^2 / df)
  temp <- sqrt(sig_sqr * solve(t(X) %*% X))
  se <- diag(temp)

  t_vals <- B/se
  p_vals <- 2*pt(abs(t_vals), df, lower.tail = FALSE)

  summary <- cbind(B, se, t_vals, p_vals)
  colnames(summary) <- c("estimate", "std error", "t-value", "p-value")
  return(summary)
}
