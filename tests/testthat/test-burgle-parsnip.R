## ##############################################################################
## parsnip::model_fit - Parsnip wrapper dispatcher
## ##############################################################################

test_that("burgle.model_fit dispatches to underlying lm", {
  skip_if_not_installed("parsnip")

  ## Fit with parsnip using the lm engine
  spec <- parsnip::linear_reg() |> parsnip::set_engine("lm")
  fit  <- parsnip::fit(spec, Sepal.Length ~ Sepal.Width + Petal.Length,
                       data = iris)

  bfit <- burgle(fit)

  ## Should produce a burgle_lm object (the underlying fit is lm)
  expect_true(inherits(bfit, "burgle_lm"))
})

test_that("burgle.model_fit lm predictions match original", {
  skip_if_not_installed("parsnip")

  spec <- parsnip::linear_reg() |> parsnip::set_engine("lm")
  fit  <- parsnip::fit(spec, Sepal.Length ~ Sepal.Width + Petal.Length,
                       data = iris)

  bfit <- burgle(fit)

  preds_lm     <- as.numeric(stats::predict(fit$fit, newdata = head(iris)))
  preds_burgle <- as.numeric(predict(bfit, newdata = head(iris),
                                     original = TRUE, draws = 1, type = "lp"))

  expect_equal(preds_burgle, preds_lm, tolerance = 1e-5)
})

test_that("burgle.model_fit dispatches to underlying glm", {
  skip_if_not_installed("parsnip")

  iris2       <- iris
  iris2$bin_y <- as.integer(iris2$Species == "versicolor")

  spec <- parsnip::logistic_reg() |> parsnip::set_engine("glm")
  fit  <- parsnip::fit(spec, bin_y ~ Sepal.Width + Petal.Length,
                       data = iris2)

  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_glm"))
})

test_that("burgle.model_fit errors when $fit is NULL", {
  skip_if_not_installed("parsnip")

  ## Construct a fake model_fit with no $fit
  fake <- structure(list(fit = NULL), class = "model_fit")
  expect_error(burgle(fake), "no \\$fit component")
})

test_that("burgle.model_fit dispatches to underlying coxph", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  spec <- parsnip::proportional_hazards() |>
    parsnip::set_engine("survival") |>
    parsnip::set_mode("censored regression")

  fit <- parsnip::fit(spec, survival::Surv(time, status) ~ age + sex,
                      data = lung)

  bfit <- burgle(fit)

  expect_true(inherits(bfit, "burgle_coxph"))
})
