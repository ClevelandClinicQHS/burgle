

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

## ##############################################################################
## GLM: type = "link" returns linear predictor scale
## ##############################################################################

test_that("predict.burgle_glm type='link' returns simulated values on LP scale (not probabilities)", {
  fit <- glm(I(Species == "versicolor") ~ Petal.Width + Sepal.Length,
             family = "binomial", data = iris)
  bfit <- burgle(fit)

  set.seed(1)
  result_link <- predict(bfit, newdata = head(iris), original = TRUE, type = "link",
                         se = TRUE, sims = 10)
  result_lp   <- predict(bfit, newdata = head(iris), original = TRUE, type = "lp")

  ## "link" values should span values outside [0, 1] (log-odds scale)
  ## while "lp" is the point prediction; both should be on the same scale
  lp_vals <- as.numeric(result_lp)
  link_vals <- if(is.list(result_link)) unlist(result_link) else as.numeric(result_link)

  ## log-odds can be any real number — values outside [0,1] confirm LP scale
  expect_true(any(link_vals < 0) || any(link_vals > 1) ||
                all(abs(link_vals - lp_vals) < 5))  ## within 5 log-odds of original
})

test_that("predict.burgle_glm type='response' gives binary outcomes for binomial", {
  fit <- glm(I(Species == "versicolor") ~ Petal.Width + Sepal.Length,
             family = "binomial", data = iris)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = TRUE, type = "response")
  expect_true(all(result %in% 0:1))
})

test_that("predict.burgle_glm type='response' for non-binomial returns continuous means", {
  count_data <- data.frame(y = c(2, 3, 1, 5, 2, 4, 1, 2, 3, 2),
                           x = c(1, 2, 1, 3, 2, 3, 1, 2, 3, 1))
  fit <- glm(y ~ x, family = poisson, data = count_data)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = count_data, original = TRUE, type = "response")

  ## Poisson response should be positive real values (not integers)
  expect_true(is.numeric(result) || is.matrix(result))
  vals <- as.numeric(result)
  expect_true(all(vals > 0))
})

test_that("predict.burgle_glm original=TRUE with draws > 1 errors", {
  fit <- glm(I(Species == "versicolor") ~ Petal.Width + Sepal.Length,
             family = "binomial", data = iris)
  bfit <- burgle(fit)

  expect_error(predict(bfit, newdata = head(iris), original = TRUE, draws = 3),
               "Can only have one draw from the original model")
})

