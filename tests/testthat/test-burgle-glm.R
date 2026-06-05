

## ##############################################################################
## GLM: Prediction Tests (original = TRUE)
## ##############################################################################

test_that("predict.burgle_glm original=TRUE matches glm link predictions", {
  fit <- glm(I(Species == "versicolor") ~ Petal.Width + Sepal.Length,
             family = "binomial", data = iris)
  bfit <- burgle(fit)

  preds_original <- stats::predict(fit, newdata = head(iris), type = "link")
  preds_burgle <- predict(bfit, newdata = head(iris), original = TRUE, type = "lp")

  expect_equal(as.numeric(preds_burgle), as.numeric(preds_original), tolerance = 1e-5)
})

test_that("predict.burgle_glm original=TRUE response type gives binary for binomial", {
  fit <- glm(I(Species == "versicolor") ~ ., family = "binomial", data = iris)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = TRUE, type = "response")

  expect_true(all(result %in% 0:1))
})

## ##############################################################################
## GLM: Multiple Draws with Predictions
## ##############################################################################

test_that("predict.burgle_glm with multiple draws gives multiple columns", {
  fit <- glm(I(Species == "versicolor") ~ Petal.Width + Sepal.Length,
             family = "binomial", data = iris)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = FALSE, draws = 3, type = "lp")

  expect_equal(ncol(result), 3)
})
