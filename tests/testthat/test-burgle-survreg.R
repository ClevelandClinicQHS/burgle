## ##############################################################################
## survival::survreg - Parametric survival regression
## ##############################################################################

test_that("burgle.survreg preserves coefficients", {
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  fit  <- survival::survreg(survival::Surv(time, status) ~ age + sex,
                             data = lung, dist = "weibull")
  bfit <- burgle(fit)

  expect_equal(bfit$coef,
               c(stats::coef(fit), "Log(scale)" = log(fit$scale)))
})

test_that("burgle.survreg stores correct dist and indices (weibull)", {
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  fit  <- survival::survreg(survival::Surv(time, status) ~ age + sex,
                             data = lung, dist = "weibull")
  bfit <- burgle(fit)

  expect_equal(bfit$dist, "weibull")
  n <- length(stats::coef(fit))
  expect_equal(bfit$loc_idx, seq_len(n))
  expect_equal(bfit$scale_idx, n + 1L)
})

test_that("burgle.survreg handles exponential (no log-scale coef)", {
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  fit  <- survival::survreg(survival::Surv(time, status) ~ age,
                             data = lung, dist = "exponential")
  bfit <- burgle(fit)

  expect_equal(bfit$dist, "exponential")
  expect_null(bfit$scale_idx)
  expect_equal(bfit$loc_idx, seq_along(stats::coef(fit)))
})

test_that("predict.burgle_survreg lp matches survreg linear predictor", {
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  fit  <- survival::survreg(survival::Surv(time, status) ~ age + sex,
                             data = lung, dist = "weibull")
  bfit <- burgle(fit)

  ## survreg linear predictor (type = "linear") = location parameter
  preds_survreg <- as.numeric(stats::predict(fit, newdata = head(lung),
                                             type = "linear"))
  preds_burgle  <- as.numeric(predict(bfit, newdata = head(lung),
                                      original = TRUE, draws = 1, type = "lp"))

  expect_equal(preds_burgle, preds_survreg, tolerance = 1e-5)
})

test_that("predict.burgle_survreg risk returns valid probabilities (weibull)", {
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  fit  <- survival::survreg(survival::Surv(time, status) ~ age + sex,
                             data = lung, dist = "weibull")
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE,
                    type = "risk", times = 500)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(head(lung)))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("predict.burgle_survreg risk with multiple time points", {
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  fit  <- survival::survreg(survival::Surv(time, status) ~ age + sex,
                             data = lung, dist = "weibull")
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE,
                    type = "risk", times = c(300, 500, 700))

  expect_equal(nrow(result), nrow(head(lung)))
  expect_equal(ncol(result), 3)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("predict.burgle_survreg risk is monotone increasing in time", {
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  fit  <- survival::survreg(survival::Surv(time, status) ~ age,
                             data = lung, dist = "weibull")
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE,
                    type = "risk", times = c(100, 300, 500, 700))

  ## Risk should be non-decreasing across time columns
  expect_true(all(result[, 2] >= result[, 1]))
  expect_true(all(result[, 3] >= result[, 2]))
  expect_true(all(result[, 4] >= result[, 3]))
})

test_that("predict.burgle_survreg response returns binary outcomes", {
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  fit  <- survival::survreg(survival::Surv(time, status) ~ age,
                             data = lung, dist = "weibull")
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE,
                    type = "response", times = 500)

  expect_true(all(result %in% 0:1))
})

test_that("predict.burgle_survreg multiple draws give multiple results", {
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  fit  <- survival::survreg(survival::Surv(time, status) ~ age,
                             data = lung, dist = "lognormal")
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = FALSE,
                    draws = 3, type = "risk", times = 500)

  expect_true(is.list(result))
  expect_equal(length(result), 3)
  expect_true(all(sapply(result, function(m) all(m >= 0 & m <= 1))))
})

test_that("predict.burgle_survreg works for lognormal distribution", {
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  fit  <- survival::survreg(survival::Surv(time, status) ~ age,
                             data = lung, dist = "lognormal")
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE,
                    type = "risk", times = 500)

  expect_true(all(result >= 0 & result <= 1))
})

test_that("predict.burgle_survreg works for exponential distribution", {
  skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status <- lung$status - 1

  fit  <- survival::survreg(survival::Surv(time, status) ~ age,
                             data = lung, dist = "exponential")
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung), original = TRUE,
                    type = "risk", times = 500)

  expect_true(all(result >= 0 & result <= 1))
})
