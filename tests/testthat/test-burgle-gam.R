## ##############################################################################
## mgcv::gam compatibility
## ##############################################################################

test_that("burgle.gam linear-only gaussian model predicts like mgcv::gam", {
  skip_if_not_installed("mgcv")

  fit <- mgcv::gam(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_lm"))

  preds_original <- stats::predict(fit, newdata = head(iris))
  preds_burgle <- predict(bfit, newdata = head(iris), original = TRUE, draws = 1, type = "lp")
  expect_equal(as.numeric(preds_burgle), as.numeric(preds_original), tolerance = 1e-5)
})

test_that("burgle.gam linear-only binomial model uses universal glm predict path", {
  skip_if_not_installed("mgcv")

  dat <- iris
  dat$bin_y <- as.integer(dat$Species == "versicolor")

  fit <- mgcv::gam(bin_y ~ Sepal.Width + Petal.Length,
                   family = stats::binomial(),
                   data = dat)
  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_glm"))
  expect_true(grepl("binomial", bfit$family))

  result <- predict(bfit, newdata = head(dat), original = TRUE, type = "response")
  expect_true(all(result %in% 0:1))
})

test_that("burgle.gam errors for smooth terms", {
  skip_if_not_installed("mgcv")

  fit <- mgcv::gam(Sepal.Length ~ s(Sepal.Width), data = iris)
  expect_error(
    burgle(fit),
    "smooth terms are not currently supported"
  )
})
