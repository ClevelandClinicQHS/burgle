## ##############################################################################
## betareg::betareg - Beta regression
## ##############################################################################

## Helper to create bounded (0,1) response data
make_beta_data <- function(n = 100, seed = 1){
  set.seed(seed)
  x <- rnorm(n)
  ## logistic function maps linear predictor to (0,1)
  mu <- stats::plogis(0.3 + 0.8 * x)
  phi <- 10
  y <- stats::rbeta(n, mu * phi, (1 - mu) * phi)
  data.frame(y = y, x = x)
}

test_that("burgle.betareg preserves coefficients", {
  skip_if_not_installed("betareg")

  df   <- make_beta_data()
  fit  <- betareg::betareg(y ~ x, data = df)
  bfit <- burgle(fit)

  expect_equal(bfit$coef, stats::coef(fit))
})

test_that("burgle.betareg stores mean_idx and prec_idx correctly", {
  skip_if_not_installed("betareg")

  df   <- make_beta_data()
  fit  <- betareg::betareg(y ~ x, data = df)
  bfit <- burgle(fit)

  n_mean <- length(fit$coefficients$mean)
  n_prec <- length(fit$coefficients$precision)

  expect_equal(bfit$mean_idx, seq_len(n_mean))
  expect_equal(bfit$prec_idx, n_mean + seq_len(n_prec))
})

test_that("predict.burgle_betareg lp returns numeric vector", {
  skip_if_not_installed("betareg")

  df   <- make_beta_data()
  fit  <- betareg::betareg(y ~ x, data = df)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(df), original = TRUE, type = "lp")

  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(head(df)))
})

test_that("predict.burgle_betareg link returns values in (0,1)", {
  skip_if_not_installed("betareg")

  df   <- make_beta_data()
  fit  <- betareg::betareg(y ~ x, data = df)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(df), original = TRUE, type = "link")

  expect_true(all(result > 0 & result < 1))
  expect_equal(length(result), nrow(head(df)))
})

test_that("predict.burgle_betareg link matches betareg predict type='response'", {
  skip_if_not_installed("betareg")

  df   <- make_beta_data()
  fit  <- betareg::betareg(y ~ x, data = df)
  bfit <- burgle(fit)

  preds_betareg <- as.numeric(stats::predict(fit, newdata = head(df),
                                              type = "response"))
  preds_burgle  <- as.numeric(predict(bfit, newdata = head(df),
                                      original = TRUE, type = "link"))

  expect_equal(preds_burgle, preds_betareg, tolerance = 1e-5)
})

test_that("predict.burgle_betareg response returns values in (0,1)", {
  skip_if_not_installed("betareg")

  df   <- make_beta_data()
  fit  <- betareg::betareg(y ~ x, data = df)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(df), original = TRUE,
                    type = "response", sims = 1)

  expect_true(all(result > 0 & result < 1))
  expect_equal(length(result), nrow(head(df)))
})

test_that("predict.burgle_betareg multiple sims returns list", {
  skip_if_not_installed("betareg")

  df   <- make_beta_data()
  fit  <- betareg::betareg(y ~ x, data = df)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(df), original = TRUE,
                    type = "response", sims = 3)

  expect_true(is.list(result))
  expect_equal(length(result), 3)
  expect_true(all(sapply(result, function(x) all(x > 0 & x < 1))))
})

test_that("predict.burgle_betareg multiple draws returns list", {
  skip_if_not_installed("betareg")

  df   <- make_beta_data()
  fit  <- betareg::betareg(y ~ x, data = df)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(df), original = FALSE,
                    draws = 3, type = "link")

  expect_true(is.list(result))
  expect_equal(length(result), 3)
})
