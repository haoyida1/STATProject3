#' My Random Forest Cross Validation
#'
#' This function outputs the cross validation mean-squared error from random forest models.
#'
#' @param k Numeric input representing the number of folds.
#'
#' @return Numeric indicating the cross validation MSE of the random forest models of fold \code{k}.
#'
#' @keywords inference
#'
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(10)
#'
#' @export
my_rf_cv <- function(k) {
  # Organize data into table and split it
  penguins <- tidyr::drop_na(palmerpenguins::penguins)
  train <- penguins[c("bill_length_mm", "bill_depth_mm", "flipper_length_mm")]
  true <- penguins["body_mass_g"]
  n = nrow(train)
  folds <- sample(rep(1:k, length = n))
  data <- data.frame(train, true, "split" = folds)

  # Empty matrix to store predictions
  predictions <- matrix(NA, n, 2)
  cv_err <- 0
  for (i in 1:k) {
    data_train <- data %>% dplyr::filter(split != i)
    data_test <- data %>% dplyr::filter(split == i)
    # Train our model
    tree_model <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = data_train, ntree = 100)
    # Record predictions
    predictions <- predict(tree_model, data_test[,-4])
    # Calculate MSE
    #print(head(data_test[,"body_mass_g"],10))
    mse <- mean((predictions - data_test[,"body_mass_g"])^2)
    cv_err <- cv_err + mse
  }

  # Finish calculating CV Error and return
  return(cv_err/k)
}
