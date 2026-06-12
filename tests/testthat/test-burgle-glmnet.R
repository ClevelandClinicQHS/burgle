## ##############################################################################
## GLMNET: Basic Structure Tests
## ##############################################################################

test_that("burgle.glmnet preserves coefficients", {
  skip_if_not_installed("glmnet")
  
  set.seed(123)
  X <- as.matrix(iris[, 1:4])
  y <- iris$Sepal.Length
  
  fit <- glmnet::glmnet(X, y, family = "gaussian")
  bfit <- burgle(fit)
  
  expect_named(bfit, c("coef", "cov", "mse", "lambda", "family", "nfeatures"))
  expect_equal(length(bfit$coef), 4)
})

test_that("burgle.glmnet sets covariance to zero (no uncertainty)", {
  skip_if_not_installed("glmnet")
  
  set.seed(123)
  X <- as.matrix(iris[, 1:4])
  y <- iris$Sepal.Length
  
  fit <- glmnet::glmnet(X, y, family = "gaussian")
  bfit <- burgle(fit)
  
  expect_equal(bfit$cov, matrix(0, nrow = 4, ncol = 4))
})

## ##############################################################################
## CV.GLMNET: Basic Structure Tests
## ##############################################################################

test_that("burgle.cv.glmnet preserves coefficients at lambda.1se", {
  skip_if_not_installed("glmnet")
  
  set.seed(123)
  X <- as.matrix(iris[, 1:4])
  y <- iris$Sepal.Length
  
  cvfit <- glmnet::cv.glmnet(X, y, family = "gaussian")
  bcvfit <- burgle(cvfit)
  
  expect_named(bcvfit, c("coef", "cov", "mse", "lambda", "lambda_choice", "family", "nfeatures", "cv_object"))
  expect_equal(bcvfit$lambda_choice, "lambda.1se")
  expect_length(bcvfit$coef, 4)
})

test_that("burgle.cv.glmnet can use lambda.min", {
  skip_if_not_installed("glmnet")
  
  set.seed(123)
  X <- as.matrix(iris[, 1:4])
  y <- iris$Sepal.Length
  
  cvfit <- glmnet::cv.glmnet(X, y, family = "gaussian")
  bcvfit <- burgle(cvfit, lambda_choice = "lambda.min")
  
  expect_equal(bcvfit$lambda_choice, "lambda.min")
  expect_equal(bcvfit$lambda, cvfit$lambda.min)
})

## ##############################################################################
## GLMNET: Prediction Tests
## ##############################################################################

test_that("predict.burgle_glmnet original=TRUE returns predictions", {
  skip_if_not_installed("glmnet")
  
  set.seed(123)
  X <- as.matrix(iris[, 1:4])
  y <- iris$Sepal.Length
  
  fit <- glmnet::glmnet(X, y, family = "gaussian")
  bfit <- burgle(fit)
  
  newdata <- as.data.frame(X[1:5, ])
  preds <- predict(bfit, newdata = newdata, original = TRUE, type = "link")
  
  expect_equal(nrow(preds), 5)
  expect_equal(ncol(preds), 1)
})

test_that("predict.burgle_glmnet with gaussian family response type", {
  skip_if_not_installed("glmnet")
  
  set.seed(123)
  X <- as.matrix(iris[, 1:4])
  y <- iris$Sepal.Length
  
  fit <- glmnet::glmnet(X, y, family = "gaussian")
  bfit <- burgle(fit)
  
  newdata <- as.data.frame(X[1:5, ])
  preds <- predict(bfit, newdata = newdata, original = TRUE, type = "response")
  
  expect_equal(nrow(preds), 5)
  expect_equal(ncol(preds), 1)
})

test_that("predict.burgle_glmnet with binomial family", {
  skip_if_not_installed("glmnet")
  
  set.seed(123)
  X <- as.matrix(iris[, 1:4])
  y <- as.numeric(iris$Species == "versicolor")
  
  fit <- glmnet::glmnet(X, y, family = "binomial")
  bfit <- burgle(fit)
  
  newdata <- as.data.frame(X[1:5, ])
  preds <- predict(bfit, newdata = newdata, original = TRUE, type = "response")
  
  expect_equal(nrow(preds), 5)
  # Binomial response should be 0 or 1
  expect_true(all(preds %in% 0:1))
})

## ##############################################################################
## CV.GLMNET: Prediction Tests
## ##############################################################################

test_that("predict.burgle_cv.glmnet original=TRUE returns predictions", {
  skip_if_not_installed("glmnet")
  
  set.seed(123)
  X <- as.matrix(iris[, 1:4])
  y <- iris$Sepal.Length
  
  cvfit <- glmnet::cv.glmnet(X, y, family = "gaussian")
  bcvfit <- burgle(cvfit)
  
  newdata <- as.data.frame(X[1:5, ])
  preds <- predict(bcvfit, newdata = newdata, original = TRUE, type = "link")
  
  expect_equal(nrow(preds), 5)
  expect_equal(ncol(preds), 1)
})

test_that("predict.burgle_cv.glmnet with binomial family", {
  skip_if_not_installed("glmnet")
  
  set.seed(123)
  X <- as.matrix(iris[, 1:4])
  y <- as.numeric(iris$Species == "versicolor")
  
  cvfit <- glmnet::cv.glmnet(X, y, family = "binomial")
  bcvfit <- burgle(cvfit)
  
  newdata <- as.data.frame(X[1:5, ])
  preds <- predict(bcvfit, newdata = newdata, original = TRUE, type = "response")
  
  expect_equal(nrow(preds), 5)
  # Binomial response should be 0 or 1
  expect_true(all(preds %in% 0:1))
})

## ##############################################################################
## GLMNET: Error Handling Tests
## ##############################################################################

test_that("predict.burgle_glmnet errors if newdata has wrong number of features", {
  skip_if_not_installed("glmnet")
  
  set.seed(123)
  X <- as.matrix(iris[, 1:4])
  y <- iris$Sepal.Length
  
  fit <- glmnet::glmnet(X, y, family = "gaussian")
  bfit <- burgle(fit)
  
  newdata_bad <- as.data.frame(X[1:5, 1:2])
  
  expect_error(predict(bfit, newdata = newdata_bad, original = TRUE, type = "link"))
})
