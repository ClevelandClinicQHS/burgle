assign("coef.flexsurvreg", function(object, ...) object$coef_values, envir = .GlobalEnv)
assign("vcov.flexsurvreg", function(object, ...) object$vcov_values, envir = .GlobalEnv)
assign("predict.mockrf", function(object, newdata = NULL, importance = FALSE, outcome = "test", ...) {
  n <- nrow(newdata)
  kind <- class(object)[3]

  if (kind == "regr") {
    return(structure(list(predicted = seq_len(n) / 10), class = c("mockpred", "result", "regr")))
  }

  if (kind == "class") {
    odds <- cbind(no = rep(0.25, n), yes = rep(0.75, n))
    return(structure(list(predicted = odds), class = c("mockpred", "result", "class")))
  }

  if (kind == "surv") {
    surv <- matrix(c(0.9, 0.7, 0.8, 0.6), nrow = n, byrow = TRUE)
    return(structure(list(survival = surv), class = c("mockpred", "result", "surv")))
  }

  cif <- array(c(0.1, 0.2, 0.3, 0.4), dim = c(n, 2, 1))
  structure(list(cif = cif), class = c("mockpred", "result", "surv-CR"))
}, envir = .GlobalEnv)

make_mock_flexsurv <- function(formula = ~ x + grp, na_cov = FALSE) {
  df <- data.frame(x = c(0, 1, 2), grp = factor(c("a", "b", "a")), ytime = c(1, 2, 3))
  terms_obj <- terms(formula, data = df)
  mf <- model.frame(formula, df)
  mm <- model.matrix(formula, df)

  coef_vals <- c(rate = log(0.2))
  if (ncol(mm) > 1) {
    coef_vals <- c(coef_vals, setNames(rep(0.1, ncol(mm) - 1L), colnames(mm)[-1]))
  }

  vc <- diag(length(coef_vals))
  if (na_cov) {
    vc[1, 1] <- NA_real_
  }

  structure(
    list(
      coef_values = coef_vals,
      vcov_values = vc,
      covdata = list(terms = terms_obj, xlev = .getXlevels(terms_obj, mf)),
      dfns = list(p = stats::pexp, H = function(x, rate) rate * x, q = stats::qexp),
      data = list(
        Y = cbind(time = df$ytime),
        mml = list(mu = structure(mm, contrasts = attr(mm, "contrasts")))
      ),
      dlist = list(inv.transforms = list(exp), location = "rate"),
      basepars = 1L
    ),
    class = "flexsurvreg"
  )
}

make_mock_rfsrc <- function(kind) {
  sampsize_fun <- local({ z <- 1; function() z })
  forest <- structure(
    list(
      xvar = matrix(1),
      yvar = if (kind %in% c("surv", "surv-CR")) matrix(c(1, 1), nrow = 1) else matrix(1),
      n = 2,
      event.info = list(time = 1:2, event = c(0, 1), cens = c(0, 1)),
      sampsize = sampsize_fun,
      time.interest = c(1, 2)
    ),
    class = c("mockrf", "forest", kind)
  )
  structure(list(forest = forest), class = "rfsrc")
}

# test_that("burgle_flexsurvreg mock object handles risk and time predictions", {
#   bfit <- burgle(make_mock_flexsurv(~ x))
#   new_data <- data.frame(x = c(1, 2))
#
#   risk <- predict(bfit, newdata = new_data, type = "risk", times = c(1, 2))
#   times <- predict(bfit, newdata = new_data, type = "time")
#
#   expect_equal(dim(risk), c(2, 2))
#   expect_true(all(risk >= 0 & risk <= 1))
#   expect_equal(length(times), 2)
# })
#
# test_that("burgle_flexsurvreg mock object validates times, levels, and NA covariance", {
#   bfit <- burgle(make_mock_flexsurv(~ grp, na_cov = TRUE))
#   new_data <- data.frame(grp = factor("c"))
#
#   expect_equal(bfit$cov, matrix(0, nrow = 2, ncol = 2))
#   expect_error(predict(bfit, newdata = new_data, type = "risk", times = 1), "new level")
#   expect_error(predict(bfit, newdata = data.frame(grp = factor("a")), type = "risk"), "times is missing")
# })
#
# test_that("burgle_rfsrc mock regression and classification behave as expected", {
#   regr <- burgle(make_mock_rfsrc("regr"))
#   class_fit <- burgle(make_mock_rfsrc("class"))
#
#   expect_equal(predict(regr, newdata = data.frame(x = 1:2), type = "response"), c(0.1, 0.2))
#   expect_warning(predict(regr, newdata = data.frame(x = 1:2), type = "risk"), "Only response is available")
#
#   odds <- predict(class_fit, newdata = data.frame(x = 1:2), type = "risk")
#   sims <- predict(class_fit, newdata = data.frame(x = 1:2), type = "response", sims = 2)
#
#   expect_equal(dim(odds), c(2, 2))
#   expect_true(all(abs(rowSums(odds) - 1) < 1e-8))
#   expect_true(is.list(sims))
#   expect_equal(length(sims), 2)
# })
#
# test_that("burgle_rfsrc mock survival and competing-risk predictions work", {
#   surv_fit <- burgle(make_mock_rfsrc("surv"))
#   cr_fit <- burgle(make_mock_rfsrc("surv-CR"))
#
#   surv_risk <- predict(surv_fit, newdata = data.frame(x = 1:2), type = "risk", times = c(1, 2))
#   cr_risk <- predict(cr_fit, newdata = data.frame(x = 1:2), type = "risk", cause = 1, times = c(1, 2))
#
#   expect_equal(dim(surv_risk), c(2, 2))
#   expect_equal(dim(cr_risk), c(2, 2))
#   expect_true(all(surv_risk >= 0 & surv_risk <= 1))
#   expect_true(all(cr_risk >= 0 & cr_risk <= 1))
# })

test_that("burgle_rfsrc mock competing-risk prediction validates missing cause or times", {
  cr_fit <- burgle(make_mock_rfsrc("surv-CR"))

  expect_error(predict(cr_fit, newdata = data.frame(x = 1:2), type = "risk", times = c(1, 2)), "Please specify")
  expect_error(predict(cr_fit, newdata = data.frame(x = 1:2), type = "risk", cause = 1), "Please specify")
})

