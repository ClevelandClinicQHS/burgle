## ##############################################################################
## ordinal::clm - Cumulative link model
## ##############################################################################

test_that("burgle.clm preserves coefficients (alpha then beta)", {
  skip_if_not_installed("ordinal")

  data("housing", package = "MASS")
  housing$Sat <- ordered(housing$Sat, levels = c("Low", "Medium", "High"))
  fit  <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  bfit <- burgle(fit)

  expected <- stats::coef(fit)  ## clm returns c(alpha, beta)
  expect_equal(bfit$coef, expected)
})

test_that("burgle.clm stores correct metadata", {
  skip_if_not_installed("ordinal")

  data("housing", package = "MASS")
  housing$Sat <- ordered(housing$Sat, levels = c("Low", "Medium", "High"))
  fit  <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  bfit <- burgle(fit)

  expect_equal(bfit$n_alpha, length(fit$alpha))
  expect_equal(bfit$n_beta,  length(fit$beta))
  expect_equal(bfit$y_levels, fit$y.levels)
  expect_equal(bfit$link, fit$link)
})

test_that("predict.burgle_clm probs match ordinal::clm predict", {
  skip_if_not_installed("ordinal")

  data("housing", package = "MASS")
  housing$Sat <- ordered(housing$Sat, levels = c("Low", "Medium", "High"))
  fit  <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  bfit <- burgle(fit)
  nd <- head(subset(housing, select = -c(Sat, Freq)))

  preds_clm    <- stats::predict(fit, newdata = nd)$fit
  preds_burgle <- predict(bfit, newdata = nd, original = TRUE, type = "probs")

  expect_equal(as.numeric(preds_burgle), as.numeric(preds_clm), tolerance = 1e-5)
})

test_that("predict.burgle_clm probs dimensions are correct", {
  skip_if_not_installed("ordinal")

  data("housing", package = "MASS")
  housing$Sat <- ordered(housing$Sat, levels = c("Low", "Medium", "High"))
  fit  <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  bfit <- burgle(fit)
  nd <- head(subset(housing, select = -c(Sat, Freq)))

  result <- predict(bfit, newdata = nd, original = TRUE, type = "probs")

  expect_equal(nrow(result), nrow(nd))
  expect_equal(ncol(result), length(levels(housing$Sat)))
})

test_that("predict.burgle_clm probs sum to 1", {
  skip_if_not_installed("ordinal")

  data("housing", package = "MASS")
  housing$Sat <- ordered(housing$Sat, levels = c("Low", "Medium", "High"))
  fit  <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  bfit <- burgle(fit)
  nd <- head(subset(housing, select = -c(Sat, Freq)))

  result <- predict(bfit, newdata = nd, original = TRUE, type = "probs")

  expect_true(all(abs(rowSums(result) - 1.0) < 1e-8))
})

test_that("predict.burgle_clm response returns valid category labels", {
  skip_if_not_installed("ordinal")

  data("housing", package = "MASS")
  housing$Sat <- ordered(housing$Sat, levels = c("Low", "Medium", "High"))
  fit  <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  bfit <- burgle(fit)
  nd <- head(subset(housing, select = -c(Sat, Freq)))

  result <- predict(bfit, newdata = nd, original = TRUE, type = "response")

  expect_true(all(result %in% levels(housing$Sat)))
  expect_equal(length(result), nrow(nd))
})

test_that("predict.burgle_clm lp is numeric", {
  skip_if_not_installed("ordinal")

  data("housing", package = "MASS")
  housing$Sat <- ordered(housing$Sat, levels = c("Low", "Medium", "High"))
  fit  <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  bfit <- burgle(fit)
  nd <- head(subset(housing, select = -c(Sat, Freq)))

  result <- predict(bfit, newdata = nd, original = TRUE, type = "lp")

  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(nd))
})

test_that("predict.burgle_clm multiple draws returns list", {
  skip_if_not_installed("ordinal")

  data("housing", package = "MASS")
  housing$Sat <- ordered(housing$Sat, levels = c("Low", "Medium", "High"))
  fit  <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  bfit <- burgle(fit)
  nd <- head(subset(housing, select = -c(Sat, Freq)))

  result <- predict(bfit, newdata = nd, original = FALSE,
                    draws = 3, type = "probs")

  expect_true(is.list(result))
  expect_equal(length(result), 3)
})

test_that("predict.burgle_clm works with probit link", {
  skip_if_not_installed("ordinal")

  data("housing", package = "MASS")
  housing$Sat <- ordered(housing$Sat, levels = c("Low", "Medium", "High"))
  fit  <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing,
                       link = "probit")
  bfit <- burgle(fit)
  nd <- head(subset(housing, select = -c(Sat, Freq)))

  result <- predict(bfit, newdata = nd, original = TRUE, type = "probs")

  expect_true(all(abs(rowSums(result) - 1.0) < 1e-8))
})
