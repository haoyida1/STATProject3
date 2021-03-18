#' My k-Nearest Neighbors Cross Validation
#'
#' This function outputs a predicted class using given covariates and the cross
#' validation error associated with 1-knn and the given k-nn.
#'
#' @param train Data frame input of variables used to predict.
#' @param cl Data frame input of true variables being predicted.
#' @param k_nn Numeric input representing the number of neighbors.
#' @param k_cv Numeric input representing the number of folds.
#'
#' @keywords inference, prediction.
#'
#' @return List object containing:
#' * `class` a vector of the predicted class Y for all observations.
#' * `cv_err` a numeric with the cross-validation misclassification error.
#'
#' @examples
#' penguins <- tidyr::drop_na(my_penguins)
#' data1 <- penguins[c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")]
#' cl1 <- penguins["species"]
#' my_knn_cv(data1, cl1, 5, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # Organize data into table and split it
  n <- nrow(train)
  x_names <- colnames(train)
  y_name <- colnames(cl)
  folds <- sample(rep(1:k_cv, length = n))
  data <- data.frame(train, cl, "split" = folds)

  # Empty matrix to store predictions, 2 columns for 2 models
  predictions <- matrix(NA, n, 2)
  prop_err <- matrix(NA, k_cv, 2)
  for (i in 1:k_cv) {
    data_train <- data %>% dplyr::filter(split != i)
    data_test <- data %>% dplyr::filter(split == i)
    r = nrow(data_test)
    # Train our models
    knn_1_cv <- class::knn(data_train[x_names], data_test[x_names], data_train[, y_name], k = 1)
    knn_max_cv <- class::knn(data_train[x_names], data_test[x_names], data_train[, y_name], k = k_nn)
    # Record predictions as factors
    predictions[folds == i, 1] <- as.character(knn_1_cv)
    predictions[folds == i, 2] <- as.character(knn_max_cv)
    # Misclassified
    count1 = 0
    count2 = 0
    for (j in 1:r) {
      if (knn_1_cv[i] != data_test[y_name][i,1]) { count1 = count1 + 1}
      if (knn_max_cv[i] != data_test[y_name][i,1]) { count2 = count2 + 1}
    }
    prop_err[i,1] = count1/r
    prop_err[i,2] = count2/r
  }

  # Calculate CV Error
  cv_err <- matrix(NA, 1, 2)
  cv_err[1,1] <- mean(prop_err[,1])
  cv_err[1,2] <- mean(prop_err[,2])

  # Return results
  result <- list("class" = predictions, "cv_err" = cv_err)
  return(result)
}
