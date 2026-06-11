test_that("burgle_lmer preserves fixed effects", {
  skip_if_not_installed("lme4")

  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  bfit <- burgle(fit)

  expect_s3_class(bfit, "burgle_lmer")
  expect_equal(bfit[["coef"]], lme4::fixef(fit))
})

test_that("predict.burgle_lmer re.form=NA matches lmer fixed-effect predictions", {
  skip_if_not_installed("lme4")

  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  bfit <- burgle(fit)
  nd <- head(lme4::sleepstudy)

  preds_original <- stats::predict(fit, newdata = nd, re.form = NA)
  preds_burgle <- predict(bfit, newdata = nd, original = TRUE, draws = 1, type = "lp", re.form = NA)

  expect_equal(as.numeric(preds_burgle), as.numeric(preds_original), tolerance = 1e-5)
})

test_that("predict.burgle_lmer re.form=NULL matches lmer on known groups", {
  skip_if_not_installed("lme4")

  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  bfit <- burgle(fit)
  nd <- head(lme4::sleepstudy)

  preds_original <- stats::predict(fit, newdata = nd, re.form = NULL)
  preds_burgle <- predict(bfit, newdata = nd, original = TRUE, draws = 1, type = "lp", re.form = NULL)

  expect_equal(as.numeric(preds_burgle), as.numeric(preds_original), tolerance = 1e-5)
})

test_that("predict.burgle_lmer with multiple draws gives multiple columns", {
  skip_if_not_installed("lme4")

  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  bfit <- burgle(fit)

  result <- predict(bfit,
                    newdata = head(lme4::sleepstudy),
                    original = FALSE,
                    draws = 4,
                    type = "lp",
                    re.form = NA)

  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), nrow(head(lme4::sleepstudy)))
})
