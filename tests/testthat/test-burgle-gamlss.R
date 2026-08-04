## ##############################################################################
## gamlss::gamlss - Generalized Additive Models for Location, Scale, and Shape
## ##############################################################################

## Helper: Gaussian response with two predictors
make_gamlss_data <- function(n = 150, seed = 42){
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y  <- 2 + 0.5 * x1 - 0.3 * x2 + rnorm(n, sd = 1.2)
  data.frame(y = y, x1 = x1, x2 = x2)
}

## Helper: Gamma (positive continuous) response
make_gamlss_gamma_data <- function(n = 100, seed = 7){
  set.seed(seed)
  x <- rnorm(n)
  mu    <- exp(1.0 + 0.4 * x)
  sigma <- 0.5
  y <- gamlss.dist::rGA(n, mu = mu, sigma = sigma)
  data.frame(y = y, x = x)
}

## Helper: beta-distributed response
make_gamlss_beta_data <- function(n = 100, seed = 3){
  set.seed(seed)
  x  <- rnorm(n)
  mu <- stats::plogis(0.2 + 0.6 * x)
  y  <- gamlss.dist::rBE(n, mu = mu, sigma = 0.3)
  data.frame(y = y, x = x)
}


## ---- burgle.gamlss basic structure ------------------------------------------

test_that("burgle.gamlss returns a burgle_gamlss object", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df  <- make_gamlss_data()
  fit <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  expect_s3_class(bfit, "burgle_gamlss")
})

test_that("burgle.gamlss preserves mu coefficients", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df  <- make_gamlss_data()
  fit <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  expect_equal(bfit$coef[bfit$idx$mu], fit$mu.coefficients)
})

test_that("burgle.gamlss preserves sigma coefficients", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df  <- make_gamlss_data()
  fit <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  expect_equal(bfit$coef[bfit$idx$sigma], fit$sigma.coefficients)
})

test_that("burgle.gamlss active_pars includes mu and sigma", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df  <- make_gamlss_data()
  fit <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  expect_true("mu"    %in% bfit$active_pars)
  expect_true("sigma" %in% bfit$active_pars)
})

test_that("burgle.gamlss covariance matrix has correct dimensions", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df  <- make_gamlss_data()
  fit <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  n <- length(bfit$coef)
  expect_equal(dim(bfit$cov), c(n, n))
})

test_that("burgle.gamlss object is smaller than original", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df  <- make_gamlss_data()
  fit <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  expect_lt(as.numeric(utils::object.size(bfit)),
            as.numeric(utils::object.size(fit)))
})


## ---- predict lp -------------------------------------------------------------

test_that("predict.burgle_gamlss lp returns numeric vector of correct length", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_data()
  fit  <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  nd     <- head(df, 10)
  result <- predict(bfit, newdata = nd, original = TRUE, type = "lp")

  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(nd))
})

test_that("predict.burgle_gamlss lp matches gamlss fitted mu linear predictor", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_data()
  fit  <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  nd <- head(df, 20)

  ## gamlss predict(..., what = "mu", type = "link") gives the linear predictor
  expected <- as.numeric(gamlss::predictAll(fit, newdata = nd,
                                            type = "link")$mu)
  result   <- as.numeric(predict(bfit, newdata = nd,
                                 original = TRUE, type = "lp"))

  expect_equal(result, expected, tolerance = 1e-5)
})


## ---- predict link -----------------------------------------------------------

test_that("predict.burgle_gamlss link returns numeric vector of correct length", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_data()
  fit  <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  nd     <- head(df, 10)
  result <- predict(bfit, newdata = nd, original = TRUE, type = "link")

  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(nd))
})

test_that("predict.burgle_gamlss link matches gamlss mu response predictions", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_data()
  fit  <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  nd <- head(df, 20)

  expected <- as.numeric(gamlss::predictAll(fit, newdata = nd,
                                            type = "response")$mu)
  result   <- as.numeric(predict(bfit, newdata = nd,
                                 original = TRUE, type = "link"))

  expect_equal(result, expected, tolerance = 1e-5)
})


## ---- predict response -------------------------------------------------------

test_that("predict.burgle_gamlss response returns numeric of correct length", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_data()
  fit  <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  nd     <- head(df, 10)
  result <- predict(bfit, newdata = nd, original = TRUE,
                    type = "response", sims = 1, seed = 1)

  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(nd))
})

test_that("predict.burgle_gamlss response multiple sims returns list", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_data()
  fit  <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  nd     <- head(df, 10)
  result <- predict(bfit, newdata = nd, original = TRUE,
                    type = "response", sims = 3, seed = 1)

  expect_true(is.list(result))
  expect_equal(length(result), 3L)
  expect_true(all(sapply(result, length) == nrow(nd)))
})


## ---- multiple draws ---------------------------------------------------------

test_that("predict.burgle_gamlss multiple draws returns list of lp vectors", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_data()
  fit  <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  nd     <- head(df, 10)
  result <- predict(bfit, newdata = nd, original = FALSE,
                    draws = 5, type = "lp", seed = 99)

  expect_true(is.list(result))
  expect_equal(length(result), 5L)
})


## ---- Gamma family -----------------------------------------------------------

test_that("burgle.gamlss works for Gamma family with log link", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_gamma_data()
  fit  <- gamlss::gamlss(y ~ x, data = df,
                         family = gamlss.dist::GA(), trace = FALSE)
  bfit <- burgle(fit)

  expect_s3_class(bfit, "burgle_gamlss")
  expect_equal(bfit$family, "GA")
})

test_that("predict.burgle_gamlss Gamma link returns positive values", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_gamma_data()
  fit  <- gamlss::gamlss(y ~ x, data = df,
                         family = gamlss.dist::GA(), trace = FALSE)
  bfit <- burgle(fit)

  nd     <- head(df, 10)
  result <- predict(bfit, newdata = nd, original = TRUE, type = "link")

  expect_true(all(result > 0))
})

test_that("predict.burgle_gamlss Gamma response returns positive values", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_gamma_data()
  fit  <- gamlss::gamlss(y ~ x, data = df,
                         family = gamlss.dist::GA(), trace = FALSE)
  bfit <- burgle(fit)

  nd     <- head(df, 10)
  result <- predict(bfit, newdata = nd, original = TRUE,
                    type = "response", sims = 1, seed = 5)

  expect_true(all(result > 0))
})


## ---- Beta family ------------------------------------------------------------

test_that("burgle.gamlss works for Beta family", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_beta_data()
  fit  <- gamlss::gamlss(y ~ x, data = df,
                         family = gamlss.dist::BE(), trace = FALSE)
  bfit <- burgle(fit)

  expect_s3_class(bfit, "burgle_gamlss")
  expect_equal(bfit$family, "BE")
})

test_that("predict.burgle_gamlss Beta link returns values in (0, 1)", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_beta_data()
  fit  <- gamlss::gamlss(y ~ x, data = df,
                         family = gamlss.dist::BE(), trace = FALSE)
  bfit <- burgle(fit)

  nd     <- head(df, 10)
  result <- predict(bfit, newdata = nd, original = TRUE, type = "link")

  expect_true(all(result > 0 & result < 1))
})


## ---- simulate_models consistency with predict -------------------------------

test_that("simulate_models.burgle_gamlss matches predict for original model", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_data()
  fit  <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)
  nd   <- head(df, 15)

  models    <- burgle:::draw_models(bfit, original = TRUE, draws = 1)
  via_pred  <- predict(bfit, newdata = nd, original = TRUE, type = "link")
  via_sim   <- burgle:::simulate_models(bfit, models = models,
                                        newdata = nd, type = "link")

  expect_equal(via_sim, via_pred, tolerance = 1e-8)
})


## ---- error handling ---------------------------------------------------------

test_that("predict.burgle_gamlss errors when draws > 1 and original = TRUE", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_data()
  fit  <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  expect_error(
    predict(bfit, newdata = head(df), original = TRUE, draws = 2),
    "Can only have one draw"
  )
})

test_that("predict.burgle_gamlss errors for non-data-frame newdata", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  df   <- make_gamlss_data()
  fit  <- gamlss::gamlss(y ~ x1 + x2, data = df, trace = FALSE)
  bfit <- burgle(fit)

  expect_error(
    predict(bfit, newdata = as.matrix(head(df)), original = TRUE, type = "lp"),
    "newdata must be an object of class data.frame"
  )
})
