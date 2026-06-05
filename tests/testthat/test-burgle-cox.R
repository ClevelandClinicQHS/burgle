## ##############################################################################
## Cox (coxph): Coefficient Preservation Tests
## ##############################################################################

test_that("burgle_coxph preserves coefficients", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
  bfit <- burgle(fit)

  expect_equal(bfit[["coef"]], stats::coef(fit))
})

## ##############################################################################
## Cox (coxph): Prediction Tests - Risk Type (original = TRUE)
## ##############################################################################

test_that("predict.burgle_coxph risk type returns valid probabilities", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE, type = "risk", times = 500)

  expect_true(all(result >= 0 & result <= 1))
  expect_equal(nrow(result), nrow(head(lung)))
})

test_that("predict.burgle_coxph risk matches riskRegression::predictRisk", {
  lung <- survival::lung |>
    transform(status = status - 1)

  ## Need x = TRUE for riskRegression::predictRisk to work
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung, x = TRUE)
  bfit <- burgle(fit)

  time_point <- 500

  ## Get predictions from burgle
  risk_burgle <- predict(bfit, newdata = head(lung), original = TRUE,
                         type = "risk", times = time_point)

  ## Get predictions from riskRegression
  risk_rr <- riskRegression::predictRisk(fit, newdata = head(lung), times = time_point)

  ## They should be close (allow small tolerance for numerical differences)
  expect_equal(as.numeric(risk_burgle), as.numeric(risk_rr), tolerance = 1e-5)
})

test_that("predict.burgle_coxph risk matches riskRegression with multiple times", {
  lung <- survival::lung |>
    transform(status = status - 1)

  ## Need x = TRUE for riskRegression::predictRisk to work
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung, x = TRUE)
  bfit <- burgle(fit)

  times <- c(300, 500, 700)

  ## Get predictions from burgle
  risk_burgle <- predict(bfit, newdata = head(lung), original = TRUE,
                         type = "risk", times = times)

  ## Get predictions from riskRegression (it returns a matrix with times as columns)
  risk_rr <- riskRegression::predictRisk(fit, newdata = head(lung), times = times)

  ## Should have same dimensions
  expect_equal(dim(risk_burgle), dim(risk_rr))

  ## Should have matching values
  expect_equal(as.numeric(risk_burgle), as.numeric(risk_rr), tolerance = 1e-5)
})

test_that("predict.burgle_coxph risk type with multiple times", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE, type = "risk", times = c(300, 500, 700))

  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), nrow(head(lung)))
  expect_true(all(result >= 0 & result <= 1))
})

## ##############################################################################
## Cox (coxph): Multiple Draws with Predictions - Risk Type
## ##############################################################################

test_that("predict.burgle_coxph with multiple draws gives multiple columns for risk", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung, x = TRUE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = FALSE, draws = 3, type = "risk", times = 500)

  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), nrow(head(lung)))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("predict.burgle_coxph multiple draws produce varied predictions", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung, x = TRUE)
  bfit <- burgle(fit)

  set.seed(123)
  result <- predict(bfit, newdata = head(lung), original = FALSE, draws = 100,
                    type = "risk", times = 500)

  ## Different draws should produce different predictions
  expect_true(all(apply(result, 1, sd) > 0))  ## Should have variation across draws
})

## ##############################################################################
## Cox (cph): Coefficient Preservation Tests
## ##############################################################################

test_that("burgle_cph preserves coefficients", {
  skip_if_not_installed("rms")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- rms::cph(survival::Surv(time, status) ~ age + sex, data = lung, x = TRUE, y = TRUE)
  bfit <- burgle(fit)

  expect_equal(bfit[["coef"]], stats::coef(fit))
})

## ##############################################################################
## Cox (cph): Prediction Tests - Risk Type (original = TRUE)
## ##############################################################################

test_that("predict.burgle_cph risk type returns valid probabilities", {
  skip_if_not_installed("rms")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- rms::cph(survival::Surv(time, status) ~ age + sex, data = lung, x = TRUE, y = TRUE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE, type = "risk", times = 500)

  expect_true(all(result >= 0 & result <= 1))
  expect_equal(nrow(result), nrow(head(lung)))
})

test_that("predict.burgle_cph risk matches riskRegression::predictRisk", {
  skip_if_not_installed("rms")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- rms::cph(survival::Surv(time, status) ~ age + sex, data = lung, x = TRUE, y = TRUE, surv = TRUE)
  bfit <- burgle(fit)

  time_point <- 500

  ## Get predictions from burgle
  risk_burgle <- predict(bfit, newdata = head(lung), original = TRUE,
                         type = "risk", times = time_point)

  ## Get predictions from riskRegression
  risk_rr <- riskRegression::predictRisk(fit, newdata = head(lung), times = time_point)

  ## They should be close
  expect_equal(as.numeric(risk_burgle), as.numeric(risk_rr), tolerance = 1e-5)
})

test_that("predict.burgle_cph risk matches riskRegression with multiple times", {
  skip_if_not_installed("rms")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- rms::cph(survival::Surv(time, status) ~ age + sex, data = lung, x = TRUE, y = TRUE)
  bfit <- burgle(fit)

  times <- c(300, 500, 700)

  ## Get predictions from burgle
  risk_burgle <- predict(bfit, newdata = head(lung), original = TRUE,
                         type = "risk", times = times)

  ## Get predictions from riskRegression
  risk_rr <- riskRegression::predictRisk(fit, newdata = head(lung), times = times)

  ## Should have same dimensions
  expect_equal(dim(risk_burgle), dim(risk_rr))

  ## Should have matching values
  expect_equal(as.numeric(risk_burgle), as.numeric(risk_rr), tolerance = 1e-5)
})

test_that("predict.burgle_cph risk type with multiple times", {
  skip_if_not_installed("rms")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- rms::cph(survival::Surv(time, status) ~ age + sex, data = lung, x = TRUE, y = TRUE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE, type = "risk", times = c(300, 500, 700))

  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), nrow(head(lung)))
  expect_true(all(result >= 0 & result <= 1))
})

## ##############################################################################
## Cox (cph): Multiple Draws with Predictions - Risk Type
## ##############################################################################

test_that("predict.burgle_cph with multiple draws gives multiple columns for risk", {
  skip_if_not_installed("rms")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- rms::cph(survival::Surv(time, status) ~ age + sex, data = lung, x = TRUE, y = TRUE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = FALSE, draws = 3, type = "risk", times = 500)

  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), nrow(head(lung)))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("predict.burgle_cph multiple draws produce varied predictions", {
  skip_if_not_installed("rms")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- rms::cph(survival::Surv(time, status) ~ age + sex, data = lung, x = TRUE, y = TRUE)
  bfit <- burgle(fit)

  set.seed(123)
  result <- predict(bfit, newdata = head(lung), original = FALSE, draws = 100,
                    type = "risk", times = 500)

  ## Different draws should produce different predictions
  expect_true(all(apply(result, 1, sd) > 0))  ## Should have variation across draws
})

