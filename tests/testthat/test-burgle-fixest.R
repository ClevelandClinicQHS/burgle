## ##############################################################################
## fixest::feols / fixest::feglm - Fixed-effects regression
## ##############################################################################

test_that("burgle.fixest preserves structural coefficients (feols)", {
  skip_if_not_installed("fixest")

  fit  <- fixest::feols(Sepal.Length ~ Sepal.Width + Petal.Length | Species,
                        data = iris)
  bfit <- burgle(fit)

  expect_equal(bfit$coef, stats::coef(fit))
})

test_that("burgle.fixest has class burgle_fixest", {
  skip_if_not_installed("fixest")

  fit  <- fixest::feols(Sepal.Length ~ Sepal.Width + Petal.Length | Species,
                        data = iris)
  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_fixest"))
})

test_that("burgle.fixest works without fixed effects (plain OLS)", {
  skip_if_not_installed("fixest")

  fit  <- fixest::feols(Sepal.Length ~ Sepal.Width + Petal.Length,
                        data = iris)
  bfit <- burgle(fit)

  expect_equal(bfit$coef, stats::coef(fit))
})

test_that("predict.burgle_fixest lp matches feols structural prediction", {
  skip_if_not_installed("fixest")

  ## Without FE so we can compare linear predictors directly
  fit  <- fixest::feols(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  preds_feols  <- as.numeric(stats::predict(fit, newdata = head(iris)))
  preds_burgle <- as.numeric(predict(bfit, newdata = head(iris),
                                     original = TRUE, draws = 1, type = "lp"))

  expect_equal(preds_burgle, preds_feols, tolerance = 1e-5)
})

test_that("predict.burgle_fixest with multiple draws gives multiple columns", {
  skip_if_not_installed("fixest")

  fit  <- fixest::feols(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = FALSE,
                    draws = 3, type = "lp")

  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), nrow(head(iris)))
})

test_that("burgle.fixest feglm stores family and inv_link", {
  skip_if_not_installed("fixest")

  iris2       <- iris
  iris2$bin_y <- as.integer(iris2$Species == "versicolor")

  fit  <- fixest::feglm(bin_y ~ Sepal.Width + Petal.Length | Species,
                        data = iris2, family = binomial)
  bfit <- burgle(fit)

  expect_true(grepl("binomial", bfit$family))
  expect_true(is.function(bfit$inv_link))
})

test_that("predict.burgle_fixest feglm link returns values in (0,1)", {
  skip_if_not_installed("fixest")

  iris2       <- iris
  iris2$bin_y <- as.integer(iris2$Species == "versicolor")

  fit  <- fixest::feglm(bin_y ~ Sepal.Width + Petal.Length,
                        data = iris2, family = binomial)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris2), original = TRUE, type = "link")

  expect_true(all(result > 0 & result < 1))
})

test_that("predict.burgle_fixest feglm response returns binary values", {
  skip_if_not_installed("fixest")

  iris2       <- iris
  iris2$bin_y <- as.integer(iris2$Species == "versicolor")

  fit  <- fixest::feglm(bin_y ~ Sepal.Width + Petal.Length,
                        data = iris2, family = binomial)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris2), original = TRUE, type = "response")

  expect_true(all(result %in% 0:1))
})
