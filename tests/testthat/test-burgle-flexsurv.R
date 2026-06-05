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

test_that("predict.burgle_flexsurv with multiple draws", {
  skip_if_not_installed("flexsurv")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ age,
                               data = lung, dist = "weibull")
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = FALSE, draws = 2, type = "lp")

  expect_equal(ncol(result), 2)
})
