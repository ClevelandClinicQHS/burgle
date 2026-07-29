## ##############################################################################
## MASS::rlm - M-type robust regression
## ##############################################################################

test_that("burgle.rlm preserves coefficients", {
  skip_if_not_installed("MASS")

  fit  <- MASS::rlm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  expect_equal(bfit$coef, stats::coef(fit))
})

test_that("burgle.rlm uses robust scale for mse", {
  skip_if_not_installed("MASS")

  fit  <- MASS::rlm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  ## mse should be s^2 (robust scale squared), not sum(resid^2)/df
  expect_equal(bfit$mse, fit$s ^ 2)
})

test_that("burgle.rlm has class burgle_rlm and burgle_lm", {
  skip_if_not_installed("MASS")

  fit  <- MASS::rlm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_rlm"))
  expect_true(inherits(bfit, "burgle_lm"))
})

test_that("predict.burgle_rlm original=TRUE matches rlm predictions (lp)", {
  skip_if_not_installed("MASS")

  fit  <- MASS::rlm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  preds_rlm    <- as.numeric(stats::predict(fit, newdata = head(iris)))
  preds_burgle <- as.numeric(predict(bfit, newdata = head(iris),
                                     original = TRUE, draws = 1, type = "lp"))

  expect_equal(preds_burgle, preds_rlm, tolerance = 1e-5)
})

test_that("predict.burgle_rlm with multiple draws gives multiple columns", {
  skip_if_not_installed("MASS")

  fit  <- MASS::rlm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = FALSE,
                    draws = 4, type = "lp")

  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), nrow(head(iris)))
})

test_that("predict.burgle_rlm response type returns numeric values", {
  skip_if_not_installed("MASS")

  fit  <- MASS::rlm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = TRUE,
                    draws = 1, sims = 3, type = "response", se = TRUE)

  expect_true(is.list(result))
  expect_equal(length(result), 3)
  expect_true(all(sapply(result, is.numeric)))
})

test_that("burgle.rlm works with factor predictors", {
  skip_if_not_installed("MASS")

  fit  <- MASS::rlm(Sepal.Length ~ Species + Petal.Width, data = iris)
  bfit <- burgle(fit)

  expect_true(!is.null(bfit$xlevels))
  expect_equal(bfit$xlevels$Species, levels(iris$Species))
})
