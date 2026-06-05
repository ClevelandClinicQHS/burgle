

## ##############################################################################
## Multinomial: Prediction Tests (original = TRUE)
## ##############################################################################

test_that("predict.burgle_multinom original=TRUE matches multinom predictions", {
  fit <- nnet::multinom(Species ~ Petal.Width + Sepal.Length, data = iris, trace = FALSE)
  bfit <- burgle(fit)

  preds_original <- stats::predict(fit, newdata = head(iris), type = "probs")
  preds_burgle <- predict(bfit, newdata = head(iris), original = TRUE, type = "odds")

  expect_equal(as.numeric(preds_burgle), as.numeric(preds_original), tolerance = 1e-5)
})

## ##############################################################################
## Multinomial: Multiple Draws with Predictions
## ##############################################################################

test_that("predict.burgle_multinom with multiple draws", {
  fit <- nnet::multinom(Species ~ Petal.Width + Sepal.Length, data = iris, trace = FALSE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = FALSE, draws = 2, type = "response")

  expect_equal(length(result), 2)
})
