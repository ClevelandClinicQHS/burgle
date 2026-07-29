## ##############################################################################
## lme4::lmer / lme4::glmer - Mixed-effects models (fixed-effect portion)
## ##############################################################################

test_that("burgle.lmerMod preserves fixed-effect coefficients", {
  skip_if_not_installed("lme4")

  fit  <- lme4::lmer(Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species),
                     data = iris)
  bfit <- burgle(fit)

  expect_equal(bfit$coef, as.numeric(lme4::fixef(fit)),
               ignore_attr = TRUE)
})

test_that("burgle.lmerMod has class burgle_lmerMod and burgle_merMod", {
  skip_if_not_installed("lme4")

  fit  <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_lmerMod"))
  expect_true(inherits(bfit, "burgle_merMod"))
  expect_equal(bfit$family, "gaussian")
})

test_that("predict.burgle_lmerMod lp matches lme4 fixed-effect prediction", {
  skip_if_not_installed("lme4")

  fit  <- lme4::lmer(Sepal.Length ~ Sepal.Width + Petal.Length + (1 | Species),
                     data = iris)
  bfit <- burgle(fit)

  ## Population-level (re.form = NA) prediction from lme4
  preds_lme4   <- as.numeric(stats::predict(fit, newdata = head(iris),
                                            re.form = NA))
  preds_burgle <- as.numeric(predict(bfit, newdata = head(iris),
                                     original = TRUE, draws = 1, type = "lp"))

  expect_equal(preds_burgle, preds_lme4, tolerance = 1e-5)
})

test_that("predict.burgle_lmerMod with multiple draws gives multiple columns", {
  skip_if_not_installed("lme4")

  fit  <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = FALSE,
                    draws = 4, type = "lp")

  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), nrow(head(iris)))
})

test_that("predict.burgle_lmerMod response type returns numeric values", {
  skip_if_not_installed("lme4")

  fit  <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), original = TRUE, draws = 1,
                    sims = 2, type = "response")

  expect_true(is.list(result))
  expect_true(all(sapply(result, is.numeric)))
})

## ---- glmerMod ---------------------------------------------------------------

test_that("burgle.glmerMod preserves fixed-effect coefficients", {
  skip_if_not_installed("lme4")

  iris2       <- iris
  iris2$bin_y <- as.integer(iris2$Species == "versicolor")

  fit  <- lme4::glmer(bin_y ~ Sepal.Width + Petal.Length + (1 | Species),
                      data = iris2, family = binomial)
  bfit <- burgle(fit)

  expect_equal(bfit$coef, as.numeric(lme4::fixef(fit)),
               ignore_attr = TRUE)
  expect_true(inherits(bfit, "burgle_glmerMod"))
  expect_true(grepl("binomial", bfit$family))
})

test_that("predict.burgle_glmerMod link returns values in (0,1)", {
  skip_if_not_installed("lme4")

  iris2       <- iris
  iris2$bin_y <- as.integer(iris2$Species == "versicolor")

  fit  <- lme4::glmer(bin_y ~ Sepal.Width + Petal.Length + (1 | Species),
                      data = iris2, family = binomial)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris2), original = TRUE, type = "link")

  expect_true(all(result > 0 & result < 1))
  expect_equal(length(result), nrow(head(iris2)))
})

test_that("predict.burgle_glmerMod response returns binary values", {
  skip_if_not_installed("lme4")

  iris2       <- iris
  iris2$bin_y <- as.integer(iris2$Species == "versicolor")

  fit  <- lme4::glmer(bin_y ~ Sepal.Width + Petal.Length + (1 | Species),
                      data = iris2, family = binomial)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris2), original = TRUE, type = "response")

  expect_true(all(result %in% 0:1))
})
