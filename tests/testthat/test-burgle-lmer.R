make_lmer_test_cases <- function() {
  sleepstudy_subset <- subset(lme4::sleepstudy, Days <= 7)

  list(
    list(
      name = "bobyqa_reml_false",
      data = lme4::sleepstudy,
      fit = lme4::lmer(
        Reaction ~ Days + (1 | Subject),
        data = lme4::sleepstudy,
        REML = FALSE,
        control = lme4::lmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
      )
    ),
    list(
      name = "nloptwrap_subset",
      data = sleepstudy_subset,
      fit = lme4::lmer(
        Reaction ~ Days + (1 | Subject),
        data = sleepstudy_subset,
        control = lme4::lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
      )
    )
  )
}

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

test_that("simulate_models.burgle_lmer re.form options match lmer predictions", {
  skip_if_not_installed("lme4")

  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  bfit <- burgle(fit)
  nd <- head(lme4::sleepstudy)
  models <- draw_models(bfit)

  preds_fixed <- simulate_models(bfit, models = models, newdata = nd, type = "lp", re.form = NA)
  preds_conditional <- simulate_models(bfit, models = models, newdata = nd, type = "lp", re.form = NULL)

  expect_equal(as.numeric(preds_fixed), as.numeric(stats::predict(fit, newdata = nd, re.form = NA)), tolerance = 1e-5)
  expect_equal(as.numeric(preds_conditional), as.numeric(stats::predict(fit, newdata = nd, re.form = NULL)), tolerance = 1e-5)
})

test_that("mixed-model lmer methods honor allow.new.levels and reject unsupported re.form values", {
  skip_if_not_installed("lme4")

  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  bfit <- burgle(fit)
  nd_new <- head(lme4::sleepstudy)
  nd_new$Subject <- factor(rep("new_subject", nrow(nd_new)))

  fixed_preds <- predict(bfit, newdata = nd_new, original = TRUE, draws = 1, type = "lp", re.form = NA)
  new_level_preds <- predict(bfit, newdata = nd_new, original = TRUE, draws = 1, type = "lp", re.form = NULL, allow.new.levels = TRUE)

  expect_equal(as.numeric(new_level_preds), as.numeric(fixed_preds), tolerance = 1e-5)
  expect_error(
    predict(bfit, newdata = nd_new, original = TRUE, draws = 1, type = "lp", re.form = NULL, allow.new.levels = FALSE),
    "Found new grouping levels"
  )
  expect_error(
    simulate_models(bfit, models = draw_models(bfit), newdata = head(lme4::sleepstudy), type = "lp", re.form = ~(1 | Subject)),
    "only supports re.form = NA"
  )
})

test_that("predict.burgle_lmer uses prediction standard error by default", {
  skip_if_not_installed("lme4")

  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  bfit <- burgle(fit)
  nd <- head(lme4::sleepstudy)
  mm <- stats::model.matrix(bfit$terms, data = nd, xlev = bfit$xlevels, contrasts.arg = bfit$contrasts)
  lp <- as.numeric(stats::predict(fit, newdata = nd, re.form = NA))
  se_conf <- sqrt(rowSums(fastmm(mm, bfit$cov) * mm))
  se_pred <- sqrt(se_conf^2 + bfit$mse)

  set.seed(11)
  expected_pred <- stats::rnorm(nrow(nd), mean = lp, sd = se_pred)
  set.seed(11)
  actual_pred <- predict(bfit, newdata = nd, original = TRUE, draws = 1, sims = 1,
                         type = "response", se = TRUE, re.form = NA)

  set.seed(11)
  expected_conf <- stats::rnorm(nrow(nd), mean = lp, sd = se_conf)

  expect_equal(as.numeric(actual_pred), expected_pred, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(as.numeric(actual_pred), expected_conf, tolerance = 1e-8)))
})

test_that("burgle lmer methods support alternate optimizers and fit inputs", {
  skip_if_not_installed("lme4")

  for (case in make_lmer_test_cases()) {
    fit <- case$fit
    bfit <- burgle(fit)
    nd <- head(case$data)
    models <- draw_models(bfit)

    expect_true(inherits(bfit, "burgle_lmer"), info = case$name)
    expect_equal(bfit[["coef"]], lme4::fixef(fit), info = case$name)

    expect_equal(
      as.numeric(predict(bfit, newdata = nd, original = TRUE, draws = 1, type = "lp", re.form = NA)),
      as.numeric(stats::predict(fit, newdata = nd, re.form = NA)),
      tolerance = 1e-5,
      info = case$name
    )
    expect_equal(
      as.numeric(predict(bfit, newdata = nd, original = TRUE, draws = 1, type = "lp", re.form = NULL)),
      as.numeric(stats::predict(fit, newdata = nd, re.form = NULL)),
      tolerance = 1e-5,
      info = case$name
    )
    expect_equal(
      as.numeric(simulate_models(bfit, models = models, newdata = nd, type = "lp", re.form = NA)),
      as.numeric(stats::predict(fit, newdata = nd, re.form = NA)),
      tolerance = 1e-5,
      info = case$name
    )
    expect_equal(
      as.numeric(simulate_models(bfit, models = models, newdata = nd, type = "lp", re.form = NULL)),
      as.numeric(stats::predict(fit, newdata = nd, re.form = NULL)),
      tolerance = 1e-5,
      info = case$name
    )
  }
})
