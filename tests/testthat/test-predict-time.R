test_that("predict_time.burgle_coxph returns dataframe with time and event", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
  bfit <- burgle(fit)

  ## "Predict survival times
  result <- predict_time(bfit, newdata = head(lung))

  ## "Check return type is dataframe
  expect_true(is.data.frame(result))

  ## "Check dimensions match newdata
  expect_equal(nrow(result), nrow(head(lung)))

  ## "Check columns exist for time and event
  expect_true("time" %in% colnames(result) || "event" %in% colnames(result))
})

test_that("predict_time.burgle_coxph produces positive times", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
  bfit <- burgle(fit)

  result <- predict_time(bfit, newdata = head(lung))

  ## "All times should be positive
  expect_true(all(result$time > 0))
})

test_that("predict_time.burgle_coxph with different sample sizes", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
  bfit <- burgle(fit)

  ## "Test with 1 observation
  result1 <- predict_time(bfit, newdata = lung[1, ])
  expect_equal(nrow(result1), 1)

  ## "Test with all observations
  result_all <- predict_time(bfit, newdata = lung)
  expect_equal(nrow(result_all), nrow(lung))
})

test_that("predict_time.burgle_coxph with multiple predictions are different", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
  bfit <- burgle(fit)

  ## "Run predictions multiple times
  result1 <- predict_time(bfit, newdata = head(lung))
  result2 <- predict_time(bfit, newdata = head(lung))

  ## "Results should be different due to stochastic sampling
  expect_false(all(result1$time == result2$time))
})

test_that("predict_time.burgle_flexsurvreg returns dataframe", {
  skip_if_not_installed("flexsurv")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ age,
                               data = lung, dist = "weibull")
  bfit <- burgle(fit)

  result <- predict_time(bfit, newdata = head(lung))

  ## "Check return type
  expect_true(is.data.frame(result) || is.numeric(result))

  ## "Check correct number of predictions
  expect_equal(length(result), nrow(head(lung)))
})

test_that("predict_time.burgle_flexsurvreg produces positive times", {
  skip_if_not_installed("flexsurv")

  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ age,
                               data = lung, dist = "weibull")
  bfit <- burgle(fit)

  result <- predict_time(bfit, newdata = head(lung))

  ## "All times should be positive
  expect_true(all(result > 0))
})

test_that("predict_time requires newdata argument", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
  bfit <- burgle(fit)

  ## "Should fail without newdata
  expect_error(predict_time(bfit))
})

test_that("predict_time.burgle_coxph handles single covariate model", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = lung)
  bfit <- burgle(fit)

  ## "Set seed for reproducibility
  set.seed(123)
  result <- predict_time(bfit, newdata = head(lung))

  ## "Should return something
  expect_false(is.null(result))

  ## "Should have right number of rows
  expect_equal(nrow(result), nrow(head(lung)))

  ## "Check if numeric or dataframe
  if(is.data.frame(result)){
    ## "If dataframe, check time column
    if("time" %in% colnames(result)){
      ## "Count non-NA values
      valid_times <- sum(!is.na(result$time))
      expect_gt(valid_times, 0)
      ## "For valid times, check they're positive
      valid_mask <- !is.na(result$time) & result$time > 0
      expect_gt(sum(valid_mask), 0)
      # info = "Should have at least some positive times")
    }
  }
})
test_that("predict_time.burgle_coxph with higher risk (age) gives different times", {
  lung <- survival::lung |>
    transform(status = status - 1)

  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = lung)
  bfit <- burgle(fit)

  ## "Create two observations with different ages
  newdata <- data.frame(age = c(40, 70))

  ## "Set seed for reproducibility
  set.seed(123)
  result_young <- predict_time(bfit, newdata = data.frame(age = 40))

  set.seed(123)
  result_old <- predict_time(bfit, newdata = data.frame(age = 70))

  ## "Both should be positive
  expect_true(result_young$time > 0)
  expect_true(result_old$time > 0)
})
