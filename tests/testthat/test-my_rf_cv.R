test_that("my_rf_cv works mathematically", {
  expect_equal(my_rf_cv(5), 120000, tolerance = 5000)
})

test_that("my_rf_cv throws error", {
  expect_error(my_rf_cv("k"))
})
