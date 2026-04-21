context("Test predict_burgle")

testthat::test_that("predict_lm() works", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  testthat::expect_equal(drop(predict(bfit, newdata = head(iris))),
                    predict(fit, newdata = head(iris)), ignore_attr = TRUE
  )

  fit2 <- lm(Sepal.Length ~ Sepal.Width * Petal.Length, data = iris)
  bfit2 <- burgle(fit2)

  testthat::expect_equal(drop(predict(bfit2, newdata = head(iris))),
                    predict(fit2, newdata = head(iris)), ignore_attr = TRUE
  )

})
