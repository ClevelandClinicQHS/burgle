## ##############################################################################
## flexsurv: Prediction Tests (original = TRUE)
## ##############################################################################

test_that("predict.burgle_flexsurv original=TRUE matches type='risk'", {
  skip_if_not_installed("flexsurv")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ age,
                               data = lung, dist = "weibull")
  bfit <- burgle(fit)

  time_point <- 500

  preds_burgle <- predict(bfit, newdata = head(lung), original = TRUE, type = "risk", times = time_point)
  preds_original <- predict(fit, newdata = head(lung), type = "survival", times = time_point)

  expect_equal(nrow(preds_burgle), nrow(head(lung)))
  ## Risk is 1 - survival
  expect_equal(preds_burgle, as.matrix(1-preds_original$.pred_survival))
})

## ##############################################################################
## flexsurv: Multiple Draws with Predictions
## ##############################################################################

test_that("predict.burgle_flexsurv with multiple draws returns list of matrices", {
  skip_if_not_installed("flexsurv")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ age,
                               data = lung, dist = "weibull")
  bfit <- burgle(fit)

  set.seed(42)
  result <- predict(bfit, newdata = head(lung), original = FALSE, draws = 3,
                    type = "risk", times = 500)

  ## Should have correct structure for multiple draws
  expect_true(is.list(result) || is.matrix(result))
  if(is.list(result)){
    expect_equal(length(result), 3)
    expect_true(all(sapply(result, function(x) all(x >= 0 & x <= 1))))
  } else {
    expect_equal(ncol(result), 3)
    expect_true(all(result >= 0 & result <= 1))
  }
})

## ##############################################################################
## flexsurv: type = "time" prediction
## ##############################################################################

test_that("predict.burgle_flexsurv type='time' returns positive times", {
  skip_if_not_installed("flexsurv")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ age,
                               data = lung, dist = "weibull")
  bfit <- burgle(fit)

  set.seed(42)
  result <- predict(bfit, newdata = head(lung), original = TRUE, type = "time")

  expect_equal(length(result), nrow(head(lung)))
  expect_true(all(result > 0))
})

## ##############################################################################
## flexsurv: Multiple distributions
## ##############################################################################

test_that("predict.burgle_flexsurv exponential distribution returns valid risk", {
  skip_if_not_installed("flexsurv")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ age,
                               data = lung, dist = "exponential")
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE, type = "risk", times = 500)

  expect_equal(nrow(result), nrow(head(lung)))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("predict.burgle_flexsurv lognormal distribution returns valid risk", {
  skip_if_not_installed("flexsurv")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ age,
                               data = lung, dist = "lognormal")
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE, type = "risk", times = 500)

  expect_equal(nrow(result), nrow(head(lung)))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("predict.burgle_flexsurv multiple times returns correct dimensions", {
  skip_if_not_installed("flexsurv")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ age,
                               data = lung, dist = "weibull")
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE, type = "risk",
                    times = c(200, 500, 800))

  expect_equal(nrow(result), nrow(head(lung)))
  expect_equal(ncol(result), 3)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("predict.burgle_flexsurv errors when times missing for risk type", {
  skip_if_not_installed("flexsurv")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ age,
                               data = lung, dist = "weibull")
  bfit <- burgle(fit)

  expect_error(predict(bfit, newdata = head(lung), original = TRUE, type = "risk"),
               "times is missing")
})
