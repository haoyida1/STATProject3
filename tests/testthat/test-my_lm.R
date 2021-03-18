test_that("my_lm works mathematically", {
  expect_is(my_lm(formula = mtcars$mpg ~ mtcars$cyl + mtcars$wt, data = mtcars), "matrix")
})

test_that("my_lm throws error", {
  expect_error(my_lm("a", data = mtcars))
  expect_error(my_lm(formula = mtcars$mpg ~ mtcars$cyl + mtcars$wt, "hello"))

})
