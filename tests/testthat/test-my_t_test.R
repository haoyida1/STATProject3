test_that("my_t_test works mathematically", {
  expect_length(my_t_test(x = mtcars$mpg, alternative = "two.sided",  mu = 20), 4)
})

test_that("my_t_test throws error", {
  expect_error(my_t_test("a", "less", 0))
  expect_error(my_t_test(mtcars$mpg, "hi", 0))
  expect_error(my_t_test(mtcars$mpg, "greater", "jjba"))
})
