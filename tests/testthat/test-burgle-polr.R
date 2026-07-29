## ##############################################################################
## MASS::polr - Proportional odds logistic regression
## ##############################################################################

test_that("burgle.polr preserves betas and zeta", {
  skip_if_not_installed("MASS")

  fit  <- MASS::polr(Species ~ Sepal.Width + Petal.Width, data = iris,
                     Hess = TRUE)
  bfit <- burgle(fit)

  expected <- c(stats::coef(fit), fit$zeta)
  expect_equal(bfit$coef, expected)
})

test_that("burgle.polr stores correct metadata", {
  skip_if_not_installed("MASS")

  fit  <- MASS::polr(Species ~ Sepal.Width + Petal.Width, data = iris,
                     Hess = TRUE)
  bfit <- burgle(fit)

  expect_equal(bfit$lev,    fit$lev)
  expect_equal(bfit$method, fit$method)
  expect_equal(bfit$n_beta, length(stats::coef(fit)))
  expect_equal(bfit$n_zeta, length(fit$zeta))
})

test_that("predict.burgle_polr probs match MASS::polr predict (original)", {
  skip_if_not_installed("MASS")

  fit  <- MASS::polr(Species ~ Sepal.Width + Petal.Width, data = iris,
                     Hess = TRUE)
  bfit <- burgle(fit)

  preds_polr   <- stats::predict(fit, newdata = head(iris), type = "probs")
  preds_burgle <- predict(bfit, newdata = head(iris), original = TRUE, type = "probs")

  expect_equal(as.numeric(preds_burgle), as.numeric(preds_polr), tolerance = 1e-5)
})

test_that("predict.burgle_polr returns correct number of columns", {
  skip_if_not_installed("MASS")

  fit  <- MASS::polr(Species ~ Sepal.Width + Petal.Width, data = iris,
                     Hess = TRUE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = TRUE, type = "probs")

  expect_equal(ncol(result), length(levels(iris$Species)))
  expect_equal(nrow(result), nrow(head(iris)))
})

test_that("predict.burgle_polr probs sum to 1", {
  skip_if_not_installed("MASS")

  fit  <- MASS::polr(Species ~ Sepal.Width + Petal.Width, data = iris,
                     Hess = TRUE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = TRUE, type = "probs")

  expect_true(all(abs(rowSums(result) - 1.0) < 1e-8))
})

test_that("predict.burgle_polr response returns valid category labels", {
  skip_if_not_installed("MASS")

  fit  <- MASS::polr(Species ~ Sepal.Width + Petal.Width, data = iris,
                     Hess = TRUE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = TRUE, type = "response")

  expect_true(all(result %in% levels(iris$Species)))
  expect_equal(length(result), nrow(head(iris)))
})

test_that("predict.burgle_polr with multiple draws returns list", {
  skip_if_not_installed("MASS")

  fit  <- MASS::polr(Species ~ Sepal.Width + Petal.Width, data = iris,
                     Hess = TRUE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = FALSE,
                    draws = 3, type = "probs")

  expect_true(is.list(result))
  expect_equal(length(result), 3)
})

test_that("predict.burgle_polr lp is numeric", {
  skip_if_not_installed("MASS")

  fit  <- MASS::polr(Species ~ Sepal.Width + Petal.Width, data = iris,
                     Hess = TRUE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = TRUE, type = "lp")

  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(head(iris)))
})
