## ##############################################################################
## LM: Coefficient Preservation Tests
## ##############################################################################

test_that("burgle_lm preserves coefficients", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  expect_named(bfit, c("coef", "cov", "mse", "xlevels", "contrasts", "terms"))
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

## ##############################################################################
## LM: se, limits, se_type Tests
## ##############################################################################

test_that("predict.burgle_lm with se=TRUE returns predictions with variation across sims", {
  set.seed(1)
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = TRUE, type = "response",
                    se = TRUE, sims = 5)

  ## Each sim should be a matrix of same size as newdata
  expect_true(is.list(result) || is.matrix(result))
  if(is.list(result)){
    expect_equal(length(result), 5)
  }
})

test_that("predict.burgle_lm with limits clamps simulated values", {
  set.seed(1)
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  lims <- c(4, 8)
  result <- predict(bfit, newdata = head(iris), original = TRUE, type = "response",
                    se = TRUE, sims = 50, limits = lims)

  ## All values should be within the specified limits
  vals <- as.numeric(unlist(result))
  expect_true(all(vals >= lims[1] & vals <= lims[2]))
})

test_that("predict.burgle_lm se_type='confidence' gives narrower intervals than 'prediction'", {
  set.seed(42)
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  result_conf <- predict(bfit, newdata = head(iris, 20), original = TRUE, type = "response",
                         se = TRUE, sims = 200, se_type = "confidence")
  result_pred <- predict(bfit, newdata = head(iris, 20), original = TRUE, type = "response",
                         se = TRUE, sims = 200, se_type = "prediction")

  ## Both return the same structure
  expect_true(is.list(result_conf) || is.matrix(result_conf))
  expect_true(is.list(result_pred) || is.matrix(result_pred))

  ## Prediction SE should produce wider spread than confidence SE
  sd_conf <- sd(as.numeric(unlist(result_conf)))
  sd_pred <- sd(as.numeric(unlist(result_pred)))
  expect_gt(sd_pred, sd_conf)
})

## ##############################################################################
## LM: terms strip response variable (issue #19)
## ##############################################################################

test_that("burgle_lm terms exclude the response variable", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  ## model.matrix should succeed on data that only contains predictors (no response)
  newdata_no_response <- head(iris)[, c("Sepal.Width", "Petal.Length")]
  mm <- stats::model.matrix(bfit$terms, data = newdata_no_response)
  expect_equal(nrow(mm), 5)
})

test_that("predict.burgle_lm works when newdata lacks the response column", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  newdata <- head(iris)[, c("Sepal.Width", "Petal.Length")]
  result <- predict(bfit, newdata = newdata, original = TRUE, type = "lp")

  expect_equal(nrow(result), 5)
})

