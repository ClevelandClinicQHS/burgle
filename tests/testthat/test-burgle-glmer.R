test_that("burgle_glmer preserves fixed effects", {
  skip_if_not_installed("lme4")

  fit <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     data = lme4::cbpp,
                     family = "binomial")
  bfit <- burgle(fit)

  expect_s3_class(bfit, "burgle_glmer")
  expect_equal(bfit[["coef"]], lme4::fixef(fit))
})

test_that("predict.burgle_glmer re.form=NA matches glmer fixed-effect link predictions", {
  skip_if_not_installed("lme4")

  fit <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     data = lme4::cbpp,
                     family = "binomial")
  bfit <- burgle(fit)
  nd <- head(lme4::cbpp)

  preds_original <- stats::predict(fit, newdata = nd, type = "link", re.form = NA)
  preds_burgle <- predict(bfit, newdata = nd, original = TRUE, draws = 1, type = "lp", re.form = NA)

  expect_equal(as.numeric(preds_burgle), as.numeric(preds_original), tolerance = 1e-5)
})

test_that("predict.burgle_glmer re.form=NULL matches glmer conditional link predictions", {
  skip_if_not_installed("lme4")

  fit <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     data = lme4::cbpp,
                     family = "binomial")
  bfit <- burgle(fit)
  nd <- head(lme4::cbpp)

  preds_original <- stats::predict(fit, newdata = nd, type = "link", re.form = NULL)
  preds_burgle <- predict(bfit, newdata = nd, original = TRUE, draws = 1, type = "lp", re.form = NULL)

  expect_equal(as.numeric(preds_burgle), as.numeric(preds_original), tolerance = 1e-5)
})

test_that("predict.burgle_glmer with multiple draws gives multiple columns", {
  skip_if_not_installed("lme4")

  fit <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     data = lme4::cbpp,
                     family = "binomial")
  bfit <- burgle(fit)
  nd <- head(lme4::cbpp)

  result <- predict(bfit, newdata = nd, original = FALSE, draws = 3, type = "lp")

  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), nrow(nd))
})

test_that("simulate_models.burgle_glmer re.form options match glmer predictions", {
  skip_if_not_installed("lme4")

  fit <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     data = lme4::cbpp,
                     family = "binomial")
  bfit <- burgle(fit)
  nd <- head(lme4::cbpp)
  models <- draw_models(bfit)

  preds_fixed <- simulate_models(bfit, models = models, newdata = nd, type = "lp", re.form = NA)
  preds_conditional <- simulate_models(bfit, models = models, newdata = nd, type = "lp", re.form = NULL)

  expect_equal(as.numeric(preds_fixed), as.numeric(stats::predict(fit, newdata = nd, type = "link", re.form = NA)), tolerance = 1e-5)
  expect_equal(as.numeric(preds_conditional), as.numeric(stats::predict(fit, newdata = nd, type = "link", re.form = NULL)), tolerance = 1e-5)
})

test_that("mixed-model glmer methods honor allow.new.levels and reject unsupported re.form values", {
  skip_if_not_installed("lme4")

  fit <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                     data = lme4::cbpp,
                     family = "binomial")
  bfit <- burgle(fit)
  nd_new <- head(lme4::cbpp)
  nd_new$herd <- factor(rep("new_herd", nrow(nd_new)))

  fixed_preds <- predict(bfit, newdata = nd_new, original = TRUE, draws = 1, type = "lp", re.form = NA)
  new_level_preds <- predict(bfit, newdata = nd_new, original = TRUE, draws = 1, type = "lp", re.form = NULL, allow.new.levels = TRUE)

  expect_equal(as.numeric(new_level_preds), as.numeric(fixed_preds), tolerance = 1e-5)
  expect_error(
    predict(bfit, newdata = nd_new, original = TRUE, draws = 1, type = "lp", re.form = NULL, allow.new.levels = FALSE),
    "Found new grouping levels"
  )
  expect_error(
    simulate_models(bfit, models = draw_models(bfit), newdata = head(lme4::cbpp), type = "lp", re.form = ~(1 | herd)),
    "only supports re.form = NA"
  )
})
