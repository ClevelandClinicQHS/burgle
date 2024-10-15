context("Test predict_burgle")

testthat::test_that("predict_lm() works", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  expect_equal(drop(predict(bfit, newdata = head(iris))),
                    predict(fit, newdata = head(iris))
  )

  fit2 <- lm(Sepal.Length ~ Sepal.Width * Petal.Length, data = iris)
  bfit2 <- burgle(fit2)

  expect_equal(drop(predict(bfit2, newdata = head(iris))),
                    predict(fit2, newdata = head(iris))
  )

})
