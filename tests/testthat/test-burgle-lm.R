## ##############################################################################
## LM: Coefficient Preservation Tests
## ##############################################################################

test_that("burgle_lm preserves coefficients", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  expect_named(bfit, c("coef", "cov", "rss", "xlevels", "contrasts", "terms"))
  expect_equal(bfit[["coef"]], stats::coef(fit))
})

test_that("burgle_lm preserves covariance matrix", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  bfit <- burgle(fit)

  expect_equal(bfit[["cov"]], stats::vcov(fit))
})

## ##############################################################################
## LM: Prediction Tests (original = TRUE)
## ##############################################################################

test_that("predict.burgle_lm original=TRUE matches lm predictions", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  preds_original <- stats::predict(fit, newdata = head(iris))
  preds_burgle <- predict(bfit, newdata = head(iris), original = TRUE, draws = 1, type = "lp")

  expect_equal(as.numeric(preds_burgle), as.numeric(preds_original), tolerance = 1e-5)
})

## ##############################################################################
## LM: Multiple Draws with Predictions
## ##############################################################################

test_that("predict.burgle_lm with multiple draws gives multiple columns", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = FALSE, draws = 5, type = "lp")

  expect_equal(ncol(result), 5)
  expect_equal(nrow(result), nrow(head(iris)))
})
