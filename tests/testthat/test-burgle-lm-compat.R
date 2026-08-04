## ##############################################################################
## LM compatibility methods for fast/large model objects
## ##############################################################################

test_that("burgle.biglm returns burgle_lm and predicts like original", {
  skip_if_not_installed("biglm")

  fit <- biglm::biglm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_lm"))

  preds_original <- stats::predict(fit, newdata = head(iris))
  preds_burgle <- predict(bfit, newdata = head(iris), original = TRUE, draws = 1, type = "lp")

  expect_equal(as.numeric(preds_burgle), as.numeric(preds_original), tolerance = 1e-5)
})

test_that("burgle.speedlm returns burgle_lm and predicts like original", {
  skip_if_not_installed("speedglm")

  fit <- speedglm::speedlm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_lm"))

  preds_original <- stats::predict(fit, newdata = head(iris))
  preds_burgle <- predict(bfit, newdata = head(iris), original = TRUE, draws = 1, type = "lp")

  expect_equal(as.numeric(preds_burgle), as.numeric(preds_original), tolerance = 1e-5)
})

test_that("burgle.speedglm returns burgle_glm for binomial models", {
  skip_if_not_installed("speedglm")

  dat <- iris
  dat$bin_y <- as.integer(dat$Species == "versicolor")

  fit <- speedglm::speedglm(bin_y ~ Sepal.Width + Petal.Length,
                            family = stats::binomial(),
                            data = dat)
  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_glm"))

  result <- predict(bfit, newdata = head(dat), original = TRUE, type = "response")
  expect_true(all(result %in% 0:1))
})

test_that("burgle.bigglm returns burgle_glm for binomial models", {
  skip_if_not_installed("biglm")

  dat <- iris
  dat$bin_y <- as.integer(dat$Species == "versicolor")

  fit <- biglm::bigglm(bin_y ~ Sepal.Width + Petal.Length,
                       family = stats::binomial(),
                       data = dat)
  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_glm"))

  result <- predict(bfit, newdata = head(dat), original = TRUE, type = "response")
  expect_true(all(result %in% 0:1))
})

test_that("burgle.fastLm returns burgle_lm and predicts linear predictor", {
  skip_if_not_installed("RcppArmadillo")

  x <- stats::model.matrix(~ Sepal.Width + Petal.Length, data = iris)
  y <- iris$Sepal.Length
  fit <- RcppArmadillo::fastLm(x, y)
  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_lm"))

  nd <- head(iris)
  preds_burgle <- predict(bfit, newdata = nd, original = TRUE, draws = 1, type = "lp")
  mm <- stats::model.matrix(~ Sepal.Width + Petal.Length, data = nd)
  preds_expected <- as.numeric(mm %*% stats::coef(fit))

  expect_equal(as.numeric(preds_burgle), preds_expected, tolerance = 1e-5)
})

test_that("burgle.fixest plain OLS is lm-compatible for universal predict", {
  skip_if_not_installed("fixest")

  fit <- fixest::feols(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_fixest"))
  expect_true(inherits(bfit, "burgle_lm"))

  preds_original <- as.numeric(stats::predict(fit, newdata = head(iris)))
  preds_burgle <- as.numeric(predict(bfit, newdata = head(iris), original = TRUE, type = "lp"))

  expect_equal(preds_burgle, preds_original, tolerance = 1e-5)
})
