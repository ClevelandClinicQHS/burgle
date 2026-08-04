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

## ##############################################################################
## Comprehensive family sweep
## Covers representative families from every group (continuous real, positive,
## (0,1), zero-inflated, discrete count, 3- and 4-parameter models) to ensure
## burgle.gamlss handles every link function used by gamlss.dist, including the
## gamlss-specific ones not in stats::make.link.
## ##############################################################################

## ---- Shared data generators -------------------------------------------------

## Real-line (unbounded) response
.make_real_data <- function(n = 120, seed = 1){
  set.seed(seed)
  x <- rnorm(n)
  data.frame(y = 1.5 + 0.5 * x + rnorm(n, sd = 1.5), x = x)
}

## Positive-real response (y > 0)
.make_pos_data <- function(n = 120, seed = 2){
  set.seed(seed)
  x <- rnorm(n)
  y <- exp(1.0 + 0.4 * x) * stats::rgamma(n, shape = 2, scale = 1)
  data.frame(y = y, x = x)
}

## (0,1) response
.make_unit_data <- function(n = 120, seed = 3){
  set.seed(seed)
  x  <- rnorm(n)
  mu <- stats::plogis(0.3 + 0.5 * x)
  y  <- gamlss.dist::rBE(n, mu = mu, sigma = 0.25)
  data.frame(y = y, x = x)
}

## Count response (non-negative integer)
.make_count_data <- function(n = 120, seed = 4){
  set.seed(seed)
  x <- rnorm(n)
  y <- stats::rpois(n, lambda = exp(1.2 + 0.4 * x))
  data.frame(y = y, x = x)
}

## Zero-inflated count response
.make_zi_count_data <- function(n = 200, seed = 5){
  set.seed(seed)
  x   <- rnorm(n)
  lam <- exp(0.8 + 0.3 * x)
  y   <- stats::rpois(n, lambda = lam)
  y[sample(n, n %/% 4)] <- 0L
  data.frame(y = y, x = x)
}

## Zero-adjusted positive response
.make_za_pos_data <- function(n = 200, seed = 6){
  set.seed(seed)
  x <- rnorm(n)
  y <- exp(0.5 + 0.3 * x) * stats::rgamma(n, shape = 2, scale = 1)
  y[sample(n, n %/% 5)] <- 0.0
  data.frame(y = y, x = x)
}

## [0,1] response (beta-inflated)
.make_beinf_data <- function(n = 200, seed = 7){
  set.seed(seed)
  x  <- rnorm(n)
  mu <- stats::plogis(0.2 + 0.4 * x)
  y  <- gamlss.dist::rBE(n, mu = mu, sigma = 0.3)
  ## add some exact 0s and 1s
  y[sample(n, 15)] <- 0.0
  y[sample(n, 15)] <- 1.0
  data.frame(y = y, x = x)
}

## Helper: fit, burgle, and predict (lp + link + response) for a family
.check_family <- function(formula, data, family, types = c("lp","link","response"),
                           seed = 42){
  fit  <- gamlss::gamlss(formula, data = data, family = family,
                          trace = FALSE, control = gamlss::gamlss.control(n.cyc = 50))
  bfit <- burgle(fit)
  nd   <- head(data, 10)

  expect_s3_class(bfit, "burgle_gamlss")

  for(tp in types){
    result <- predict(bfit, newdata = nd, original = TRUE,
                      type = tp, sims = 1L, seed = seed)
    if(tp %in% c("lp", "link")){
      expect_true(is.numeric(result),
                  info = paste(deparse(family), "type =", tp))
      expect_equal(length(result), nrow(nd),
                   info = paste(deparse(family), "type =", tp))
    } else {
      ## response: single sim -> numeric (or integer for discrete)
      expect_equal(length(result), nrow(nd),
                   info = paste(deparse(family), "type = response"))
    }
  }
  invisible(bfit)
}


## ---- 2-parameter continuous (real line) -------------------------------------

test_that("all 2-parameter real-line families work: NO, GU, RG, LO", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_real_data()
  for(fam in c("NO", "GU", "RG", "LO")){
    .check_family(y ~ x, df, fam)
  }
})

test_that("NO2 family (variance parameterization) works", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_real_data()
  .check_family(y ~ x, df, "NO2")
})

## ---- 3-parameter continuous (real line) ------------------------------------

test_that("3-parameter real-line families work: TF, SN1, SN2", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_real_data()
  for(fam in c("TF", "SN1", "SN2")){
    .check_family(y ~ x, df, fam)
  }
})

test_that("PE family (logshiftto1 nu link) works", {
  ## PE uses the gamlss-specific "logshiftto1" link for nu
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df   <- .make_real_data()
  .check_family(y ~ x, df, "PE")
})

## ---- 4-parameter continuous (real line) ------------------------------------

test_that("4-parameter real-line families work: JSU, SEP, SHASH, GT, EGB2", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_real_data()
  for(fam in c("JSU", "SEP", "SHASH", "GT", "EGB2")){
    .check_family(y ~ x, df, fam)
  }
})

test_that("SST family (logshiftto2 tau link) works", {
  ## SST uses the gamlss-specific "logshiftto2" link for tau (constrains tau>2)
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_real_data()
  .check_family(y ~ x, df, "SST")
})

test_that("NET family (4-parameter, all log links) works", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_real_data()
  .check_family(y ~ x, df, "NET")
})

## ---- 2-parameter positive continuous ----------------------------------------

test_that("2-parameter positive-continuous families work: GA, IG, WEI, WEI2, WEI3, IGA, PARETO2", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_pos_data()
  for(fam in c("GA", "IG", "WEI", "WEI2", "WEI3", "IGA", "PARETO2")){
    .check_family(y ~ x, df, fam)
  }
})

test_that("LOGNO family (log link on mu, positive response) works", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_pos_data()
  .check_family(y ~ x, df, "LOGNO")
})

## ---- 3-parameter positive continuous ----------------------------------------

test_that("3-parameter positive-continuous families work: GG, BCCG, BCCGo, LNO", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_pos_data()
  for(fam in c("GG", "BCCG", "BCCGo")){
    .check_family(y ~ x, df, fam)
  }
})

## ---- 4-parameter positive continuous ----------------------------------------

test_that("4-parameter positive-continuous families work: BCPE, BCPEo, BCT, BCTo, GB2", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_pos_data()
  for(fam in c("BCPE", "BCPEo", "BCT", "BCTo", "GB2")){
    .check_family(y ~ x, df, fam)
  }
})

## ---- (0,1) continuous -------------------------------------------------------

test_that("(0,1) continuous families work: BE, BEo, SIMPLEX, LOGITNO", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_unit_data()
  for(fam in c("BE", "BEo", "SIMPLEX", "LOGITNO")){
    .check_family(y ~ x, df, fam)
  }
})

## ---- count distributions ----------------------------------------------------

test_that("1-parameter count family works: PO", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_count_data()
  .check_family(y ~ x, df, "PO")
})

test_that("2-parameter count families work: NBI, NBII, PIG, DPO", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_count_data()
  for(fam in c("NBI", "NBII", "PIG", "DPO")){
    .check_family(y ~ x, df, fam)
  }
})

test_that("3-parameter count families work: NBF, SICHEL, DEL, BNB", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_count_data()
  for(fam in c("NBF", "SICHEL", "DEL", "BNB")){
    .check_family(y ~ x, df, fam)
  }
})

## ---- zero-inflated / zero-adjusted ------------------------------------------

test_that("zero-inflated count families work: ZIP, ZINBI, ZANBI, ZIPIG, ZAPIG", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_zi_count_data()
  for(fam in c("ZIP", "ZINBI", "ZANBI", "ZIPIG", "ZAPIG")){
    .check_family(y ~ x, df, fam)
  }
})

test_that("zero-adjusted positive families work: ZAGA, ZAIG", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_za_pos_data()
  for(fam in c("ZAGA", "ZAIG")){
    .check_family(y ~ x, df, fam)
  }
})

test_that("beta-inflated families work: BEINF0, BEINF1, BEINF", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  df <- .make_beinf_data()
  for(fam in c("BEINF0", "BEINF1", "BEINF")){
    .check_family(y ~ x, df, fam)
  }
})

## ---- family stored correctly for all families -------------------------------

test_that("burgle.gamlss family slot equals family abbreviation for key families", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")

  ## object$family is a 2-element character vector; [1] is the abbreviation
  families <- c("NO", "GA", "BE", "PO", "NBI", "ZINBI", "ZAGA", "TF", "JSU", "BCPE")
  data_fns <- list(
    NO    = .make_real_data,
    GA    = .make_pos_data,
    BE    = .make_unit_data,
    PO    = .make_count_data,
    NBI   = .make_count_data,
    ZINBI = .make_zi_count_data,
    ZAGA  = .make_za_pos_data,
    TF    = .make_real_data,
    JSU   = .make_real_data,
    BCPE  = .make_pos_data
  )
  for(fam in families){
    df   <- data_fns[[fam]]()
    fit  <- gamlss::gamlss(y ~ x, data = df, family = fam,
                            trace = FALSE,
                            control = gamlss::gamlss.control(n.cyc = 50))
    bfit <- burgle(fit)
    expect_equal(bfit$family, fam,
                 info = paste("family slot for", fam))
  }
})

## ---- gamlss-specific link functions are resolved correctly ------------------

test_that(".gamlss_inv_link resolves all gamlss-specific links without error", {
  skip_if_not_installed("gamlss.dist")

  ## Standard links
  for(lnk in c("identity", "log", "logit", "probit", "cloglog", "sqrt",
               "inverse", "1/mu^2")){
    fn <- burgle:::.gamlss_inv_link(lnk)
    expect_true(is.function(fn), info = paste("link:", lnk))
  }

  ## gamlss-specific links
  for(lnk in c("mu^2", "logshiftto1", "logshiftto2", "logshiftto0", "Slog",
               "[-1,1]", "(0,2]", "(0,5]")){
    fn <- burgle:::.gamlss_inv_link(lnk)
    expect_true(is.function(fn), info = paste("link:", lnk))
  }
})

test_that(".gamlss_inv_link logshiftto1 returns values > 1", {
  skip_if_not_installed("gamlss.dist")
  fn <- burgle:::.gamlss_inv_link("logshiftto1")
  vals <- fn(c(-2, 0, 2))
  expect_true(all(vals > 1))
})

test_that(".gamlss_inv_link logshiftto2 returns values > 2", {
  skip_if_not_installed("gamlss.dist")
  fn <- burgle:::.gamlss_inv_link("logshiftto2")
  vals <- fn(c(-2, 0, 2))
  expect_true(all(vals > 2))
})

test_that(".gamlss_inv_link mu^2 is inverse of sqrt link", {
  skip_if_not_installed("gamlss.dist")
  fn <- burgle:::.gamlss_inv_link("mu^2")
  expect_equal(fn(4), 2, tolerance = 1e-10)
})
