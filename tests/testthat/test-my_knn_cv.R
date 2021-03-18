penguins <- tidyr::drop_na(palmerpenguins::penguins)
data1 <- penguins[c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")]
cl1 <- penguins["species"]

test_that("my_knn_cv works mathematically", {
  expect_length(my_knn_cv(data1, cl1, 5, 5), 2)
  expect_length(my_knn_cv(data1, cl1, 5, 10), 2)
})

test_that("my_knn_cv throws error", {
  expect_error(my_knn_cv("data1", cl1, 5, 5))
  expect_error(my_knn_cv(penguins, cl1, "llll", 5))
  expect_error(my_knn_cv(penguins, cl1, 5, "he"))
})
